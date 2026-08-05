/*
 * Copyright 2015-2026 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.previewWindow;

import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;

//---------------------------------------------------------------------------

/**
 * Represents one office-to-PDF conversion of a specific document (content-keyed:
 * all panes and dialogs requesting the same unmodified file share one session).
 * Coordinates one display consumer per wrapper (each will load the resulting
 * PDF) and any number of extraction consumers (e.g., FTS text extraction).
 *
 * <p>Consumers attach via {@link #subscribeDisplay} or {@link #subscribeExtraction}
 * and are notified when the session transitions between states. A session stays
 * alive while at least one subscriber holds a reference; when the last
 * subscriber unsubscribes and the session is still in a non-terminal state, it
 * transitions to {@link ConversionState#CANCELLED}.
 *
 * <p>State transitions from {@link ConversionState#PENDING}/{@link ConversionState#CONVERTING} to
 * a terminal state are performed by package-private methods
 * ({@link #markConverting}, {@link #complete}, {@link #fail}, {@link #cancel}).
 * In production {@code DocumentArtifactService}'s conversion worker drives
 * these; tests call them directly.
 *
 * <p>Thread safety: all state mutations are synchronized on an internal lock.
 * DisplayCallbacks are always dispatched on the JavaFX application thread
 * (via {@link org.hypernomicon.util.Util#runInFXThread(Runnable)}, which runs
 * the callback directly if already on the FX thread or schedules it via
 * {@link Platform#runLater} otherwise). Extraction subscribers receive
 * notifications via {@link CompletableFuture#complete}/{@link CompletableFuture#completeExceptionally};
 * those callbacks run on whichever executor the subscriber chained onto.
 */
public final class ConversionSession
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Lifecycle state of a conversion session. PENDING and CONVERTING are
   * non-terminal; COMPLETED, FAILED, and CANCELLED are terminal and no
   * further transitions occur.
   */
  public enum ConversionState
  {
    PENDING, CONVERTING, COMPLETED, FAILED, CANCELLED;

    /** True once the conversion has reached an end state (COMPLETED, FAILED, or CANCELLED); no further transitions occur. */
    public boolean isTerminal()    { return (this == COMPLETED) || (this == FAILED) || (this == CANCELLED); }

    /** True while the conversion is still in progress (PENDING or CONVERTING). */
    public boolean isNonTerminal() { return isTerminal() == false; }
  }

//---------------------------------------------------------------------------

  /**
   * Receives state-change notifications from a subscribed display consumer's
   * perspective. Always invoked on the JavaFX application thread.
   *
   * <p>The {@code convertedPath} parameter is non-null iff {@code state} is
   * {@link ConversionState#COMPLETED}. The {@code failure} parameter is non-null iff
   * {@code state} is {@link ConversionState#FAILED} or {@link ConversionState#CANCELLED}.
   */
  @FunctionalInterface
  public interface DisplayCallback
  {
    void onStateChange(ConversionState state, FilePath convertedPath, Throwable failure);
  }

//---------------------------------------------------------------------------

  /**
   * Handle for detaching a subscriber from a session. Idempotent; safe to call
   * {@link #unsubscribe} more than once. Implements {@link AutoCloseable} so
   * subscriptions can be used with try-with-resources.
   */
  @FunctionalInterface
  public interface Subscription extends AutoCloseable
  {
    void unsubscribe();
    @Override default void close() { unsubscribe(); }
  }

//---------------------------------------------------------------------------

  /**
   * Failure cause used when a conversion cannot be attempted because no office
   * installation is configured in settings. {@code OfficePreviewer} fails the
   * session with this so that consumers observing the failure (at transition
   * time or via terminal-state replay when subscribing late) can show the
   * specific no-office message rather than the generic unable-to-preview
   * indicator, and so consumers that don't drive a display know to stay quiet;
   * this is a settings condition, not an error.
   */
  @SuppressWarnings("serial")
  public static final class NoOfficeInstallationException extends IOException
  {
    NoOfficeInstallationException() { super("No office installation configured"); }
  }

//---------------------------------------------------------------------------

  private record DisplaySub(Object consumerKey, DisplayCallback callback) { }

  private record ExtractionSub(CompletableFuture<FilePath> future) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Object lock = new Object();

  private final FilePath source;
  private final boolean convertToHtml;
  private final Consumer<ConversionSession> onAbandoned;
  private final Set<ExtractionSub> extractionSubs = new LinkedHashSet<>();

  /** One display slot per consumer (panes key by their wrapper, dialogs by
   *  their host): content-keyed sessions can serve several consumers at once,
   *  and re-subscribing under the same key displaces only that consumer's
   *  previous subscription. */
  private final Map<Object, DisplaySub> displaySubs = new HashMap<>();

  private ConversionState state = ConversionState.PENDING;
  private FilePath convertedPath;
  private Throwable failure;
  private int leaseCount = 0;

  private DocumentArtifactService.ArtifactKey artifactKey;

//---------------------------------------------------------------------------

  /**
   * Package-private constructor; sessions are created via
   * {@code DocumentArtifactService.getOrCreateSession}.
   *
   * @param source         the office document to convert
   * @param mimetypeStr    MIME type of the source (used to decide between
   *                       PDF and HTML output)
   * @param onAbandoned    invoked once when the session loses its last subscriber;
   *                       the owner typically uses this to remove the session from
   *                       its registry. May be {@code null} in tests.
   */
  ConversionSession(FilePath source, String mimetypeStr, Consumer<ConversionSession> onAbandoned)
  {
    this.source        = Objects.requireNonNull(source, "source");
    this.convertToHtml = determineConvertToHtml(Objects.requireNonNull(mimetypeStr, "mimetypeStr"));
    this.onAbandoned   = onAbandoned;
  }

//---------------------------------------------------------------------------

  public FilePath source() { return source; }

  public ConversionState state()   { synchronized (lock) { return state; } }
  public Throwable       failure() { synchronized (lock) { return failure; } }

  FilePath convertedPath() { synchronized (lock) { return convertedPath; } }

  boolean convertToHtml()  { return convertToHtml; }

  DocumentArtifactService.ArtifactKey artifactKey() { return artifactKey; }

  void setArtifactKey(DocumentArtifactService.ArtifactKey key) { artifactKey = key; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Takes out a lease on this session's artifact, preventing cache eviction
   * from deleting the artifact file while a viewer is displaying it. Balanced
   * by {@link #release()}.
   */
  public void lease() { synchronized (lock) { leaseCount++; } }

  /** Releases a lease taken by {@link #lease()}. */
  public void release() { synchronized (lock) { if (leaseCount > 0) leaseCount--; } }

  /** Whether the artifact cache may evict this session and delete its artifact:
   *  no leases and no subscribers of either kind. */
  boolean isEvictable() { synchronized (lock) { return (leaseCount == 0) && displaySubs.isEmpty() && extractionSubs.isEmpty(); } }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether the given MIME type should be converted to HTML
   * (spreadsheets, CSV) rather than PDF. The types listed here are a subset of
   * the office-doc types {@code OfficePreviewer.isOfficeConvertible}
   * recognizes; this helper specifically identifies the ones that JodConverter
   * should render as HTML instead of PDF.
   */
  static boolean determineConvertToHtml(String mimetypeStr)
  {
    return mimetypeStr.contains("spreadsheetml.sheet")
      ||   mimetypeStr.contains("ms-excel")
      ||   "text/csv".equalsIgnoreCase(mimetypeStr)
      ||   mimetypeStr.contains("tab-separated-values")
      ||   mimetypeStr.contains("opendocument.spreadsheet")
      ||   mimetypeStr.contains("sun.xml.calc");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Attach a display consumer. Each consumer key (a pane host's wrapper, a
   * dialog host) holds one display slot; subscribing again under the same key
   * silently displaces that consumer's previous subscription (the displaced
   * subscriber receives no further notifications but is not told anything;
   * displacement is an internal housekeeping event, not a session-level
   * cancellation, so subscribers that would otherwise call {@code setUnable}
   * on CANCELLED don't fire spuriously). Different consumers' subscriptions
   * coexist: a content-keyed session can serve several panes.
   *
   * <p>If the session is already in a terminal state, the callback is fired
   * immediately on the FX thread (directly if already on it, otherwise via
   * {@link Platform#runLater}) and the returned {@link Subscription} is still
   * valid (its {@link Subscription#unsubscribe} is a no-op after terminal).
   *
   * <p>Callers are responsible for any UI side effects (showing conversion
   * progress, loading the PDF, etc.) via their {@link DisplayCallback}. The
   * session itself performs no UI-facing calls on the consumer.
   */
  public Subscription subscribeDisplay(Object consumerKey, DisplayCallback callback)
  {
    Objects.requireNonNull(callback, "callback");

    DisplaySub newSub = new DisplaySub(consumerKey, callback);

    ConversionState snapState;
    FilePath        snapPath;
    Throwable       snapFailure;

    synchronized (lock)
    {
      displaySubs.put(consumerKey, newSub);
      snapState   = state;
      snapPath    = convertedPath;
      snapFailure = failure;
    }

    // Notify the newcomer of the current state (or an immediate terminal).
    // The consumer's previous subscriber, if any, is silently displaced.

    fireDisplay(newSub, snapState, snapPath, snapFailure);

    return () -> unsubscribeDisplay(newSub);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void unsubscribeDisplay(DisplaySub sub)
  {
    boolean abandoned = false;

    synchronized (lock)
    {
      if (displaySubs.get(sub.consumerKey()) == sub)
      {
        displaySubs.remove(sub.consumerKey());
        abandoned = displaySubs.isEmpty() && extractionSubs.isEmpty();
      }
    }

    if (abandoned)
      cancelIfNonTerminal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Attach an extraction consumer. The returned future completes with the
   * converted path on {@link ConversionState#COMPLETED}, or exceptionally on
   * {@link ConversionState#FAILED} / {@link ConversionState#CANCELLED}.
   *
   * <p>The caller unsubscribes by cancelling the returned future
   * ({@code future.cancel(true)}) or by letting it complete normally.
   * When the last subscriber leaves and the session is still non-terminal,
   * the session transitions to {@link ConversionState#CANCELLED}.
   *
   * <p>If the session is already terminal, the returned future is completed
   * immediately.
   */
  public CompletableFuture<FilePath> subscribeExtraction()
  {
    CompletableFuture<FilePath> future = new CompletableFuture<>();
    ExtractionSub sub = new ExtractionSub(future);

    ConversionState snapState;
    FilePath        snapPath;
    Throwable       snapFailure;

    synchronized (lock)
    {
      extractionSubs.add(sub);
      snapState   = state;
      snapPath    = convertedPath;
      snapFailure = failure;
    }

    // If already terminal, deliver the result now.

    fireExtraction(sub, snapState, snapPath, snapFailure);

    // Whether the future is completed by us (state transition) or by the
    // caller (external cancellation), remove from the subscriber set and
    // possibly trigger session cancellation.

    future.whenComplete((path, err) ->
    {
      boolean abandoned;

      synchronized (lock)
      {
        extractionSubs.remove(sub);
        abandoned = displaySubs.isEmpty() && extractionSubs.isEmpty();
      }

      if (abandoned)
        cancelIfNonTerminal();
    });

    return future;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Package-private transition methods. Called by the conversion worker (in
// production) and by unit tests directly.

  /**
   * Transition PENDING -> CONVERTING. No-op if already past PENDING.
   * Subscribers are notified via their display callbacks.
   */
  void markConverting()
  {
    DisplaySub[] snapDisplays;
    boolean      changed = false;

    synchronized (lock)
    {
      if (state == ConversionState.PENDING)
      {
        state   = ConversionState.CONVERTING;
        changed = true;
      }

      snapDisplays = displaySubs.values().toArray(DisplaySub[]::new);
    }

    if (changed)
      for (DisplaySub sub : snapDisplays)
        fireDisplay(sub, ConversionState.CONVERTING, null, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Transition to COMPLETED with the given converted path. No-op if already
   * terminal. All current subscribers are notified.
   */
  void complete(FilePath path)
  {
    Objects.requireNonNull(path, "path");

    DisplaySub[] snapDisplays;
    ExtractionSub[] snapExtractions;
    boolean changed = false;

    synchronized (lock)
    {
      if (state.isNonTerminal())
      {
        state         = ConversionState.COMPLETED;
        convertedPath = path;
        changed       = true;
      }

      snapDisplays    = displaySubs.values().toArray(DisplaySub[]::new);
      snapExtractions = extractionSubs.toArray(ExtractionSub[]::new);
    }

    if (changed == false) return;

    for (DisplaySub sub : snapDisplays)
      fireDisplay(sub, ConversionState.COMPLETED, path, null);

    for (ExtractionSub sub : snapExtractions)
      fireExtraction(sub, ConversionState.COMPLETED, path, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Transition to FAILED with the given cause. No-op if already terminal.
   */
  void fail(Throwable cause)
  {
    Objects.requireNonNull(cause, "cause");

    DisplaySub[] snapDisplays;
    ExtractionSub[] snapExtractions;
    boolean changed = false;

    synchronized (lock)
    {
      if (state.isNonTerminal())
      {
        state   = ConversionState.FAILED;
        failure = cause;
        changed = true;
      }

      snapDisplays    = displaySubs.values().toArray(DisplaySub[]::new);
      snapExtractions = extractionSubs.toArray(ExtractionSub[]::new);
    }

    if (changed == false) return;

    for (DisplaySub sub : snapDisplays)
      fireDisplay(sub, ConversionState.FAILED, null, cause);

    for (ExtractionSub sub : snapExtractions)
      fireExtraction(sub, ConversionState.FAILED, null, cause);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Transition to CANCELLED. No-op if already terminal. Uses a generic
   * {@link CancellationException}; callers that want a specific reason should
   * construct the exception themselves and call {@link #fail} with it, choosing
   * FAILED semantics instead.
   */
  void cancel()
  {
    cancel(new CancellationException("Session cancelled"));
  }

  void cancel(CancellationException cause)
  {
    Objects.requireNonNull(cause, "cause");

    DisplaySub[] snapDisplays;
    ExtractionSub[] snapExtractions;
    boolean changed = false;

    synchronized (lock)
    {
      if (state.isNonTerminal())
      {
        state   = ConversionState.CANCELLED;
        failure = cause;
        changed = true;
      }

      snapDisplays    = displaySubs.values().toArray(DisplaySub[]::new);
      snapExtractions = extractionSubs.toArray(ExtractionSub[]::new);
    }

    if (changed == false) return;

    for (DisplaySub sub : snapDisplays)
      fireDisplay(sub, ConversionState.CANCELLED, null, cause);

    for (ExtractionSub sub : snapExtractions)
      fireExtraction(sub, ConversionState.CANCELLED, null, cause);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * If the session is non-terminal and has lost all subscribers, transition
   * to CANCELLED and notify the owner via {@code onAbandoned}.
   */
  private void cancelIfNonTerminal()
  {
    boolean shouldCancel;

    synchronized (lock)
    {
      shouldCancel = state.isNonTerminal() && displaySubs.isEmpty() && extractionSubs.isEmpty();
    }

    if (shouldCancel)
      cancel(new CancellationException("All subscribers unsubscribed"));

    if (onAbandoned != null)
      onAbandoned.accept(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void fireDisplay(DisplaySub sub, ConversionState state, FilePath path, Throwable failure)
  {
    runInFXThread(() -> sub.callback().onStateChange(state, path, failure));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void fireExtraction(ExtractionSub sub, ConversionState state, FilePath path, Throwable failure)
  {
    CompletableFuture<FilePath> future = sub.future();

    if (future.isDone()) return;

    switch (state)
    {
      case COMPLETED -> future.complete(path);
      case FAILED, CANCELLED -> future.completeExceptionally(failure);
      default -> { /* PENDING / CONVERTING: no extraction delivery yet */ }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
