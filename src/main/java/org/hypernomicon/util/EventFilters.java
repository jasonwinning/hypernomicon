/*
 * Copyright 2015-2025 Jason Winning
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

package org.hypernomicon.util;

import javafx.event.*;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.stage.Window;

import java.util.*;

//---------------------------------------------------------------------------

/**
 * Utility class for managing JavaFX event filters on various targets
 * ({@link Node}, {@link Scene}, {@link Window}).
 * <p>
 * Filters are tracked internally so they can be replaced, removed,
 * or queried without duplicating handlers for the same event type.
 * <p>
 * This class is thread-unsafe and intended for use on the JavaFX
 * Application Thread only.
 */
public final class EventFilters
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventFilters() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }


//---------------------------------------------------------------------------

  // target -> (eventType -> handler)
  private static final Map<Object, Map<EventType<?>, EventHandler<? super Event>>> FILTERS = new WeakHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Replaces any existing event filter for the specified target and event type
   * with the given handler.
   * <p>
   * If a filter is already installed for the type, it is removed before
   * the new one is added. If the given handler is identical to the current one,
   * no action is taken.
   *
   * @param target  the event source ({@link Node}, {@link Scene}, or {@link Window})
   * @param type    the event type to filter
   * @param handler the event handler to install
   * @param <E>     the event subclass
   * @throws NullPointerException     if target, type, or handler is {@code null}
   * @throws IllegalArgumentException if target is not supported
   */
  public static <E extends Event> void replaceFilter(Object target, EventType<E> type, EventHandler<? super E> handler)
  {
    Objects.requireNonNull(target);
    Objects.requireNonNull(type);
    Objects.requireNonNull(handler);

    var byType = FILTERS.computeIfAbsent(target, _ -> new HashMap<>());

    var old = byType.get(type);
    if (old == handler) return;

    if (old != null) removeFilterInternal(target, type, old);

    addFilterInternal(target, type, handler);
    byType.put(type, cast(handler));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes and unregisters any event filter for the specified target and type.
   *
   * @param target the event source ({@link Node}, {@link Scene}, or {@link Window})
   * @param type   the event type whose filter should be cleared
   * @param <E>    the event subclass
   * @throws NullPointerException if target or type is {@code null}
   */
  public static <E extends Event> void clearFilter(Object target, EventType<E> type)
  {
    Objects.requireNonNull(target);
    Objects.requireNonNull(type);

    var byType = FILTERS.get(target);
    if (byType == null) return;

    var old = byType.remove(type);
    if (old != null) removeFilterInternal(target, type, old);

    if (byType.isEmpty()) FILTERS.remove(target);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the currently installed filter for the given target and type,
   * or {@code null} if none is installed.
   *
   * @param target the event source ({@link Node}, {@link Scene}, or {@link Window})
   * @param type   the event type
   * @param <E>    the event subclass
   * @return the installed filter, or {@code null} if absent
   */
  public static <E extends Event> EventHandler<? super E> getFilter(Object target, EventType<E> type)
  {
    return Optional.ofNullable(FILTERS.get(target)).map(m -> m.get(type)).orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Installs the given event filter if and only if no filter is already installed
   * for the specified target and event type.
   *
   * @param target  the event source ({@link Node}, {@link Scene}, or {@link Window})
   * @param type    the event type to filter
   * @param handler the event handler to install
   * @param <E>     the event subclass
   * @return {@code true} if the filter was installed, {@code false} if one was already present
   * @throws NullPointerException     if target, type, or handler is {@code null}
   * @throws IllegalArgumentException if target is not supported
   */
  public static <E extends Event> boolean addFilterIfAbsent(Object target, EventType<E> type, EventHandler<? super E> handler)
  {
    Objects.requireNonNull(target);
    Objects.requireNonNull(type);
    Objects.requireNonNull(handler);

    var byType = FILTERS.computeIfAbsent(target, _ -> new HashMap<>());

    if (byType.containsKey(type)) return false;

    addFilterInternal(target, type, handler);
    byType.put(type, cast(handler));
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <E extends Event> void addFilterInternal(Object target, EventType<E> type, EventHandler<? super E> handler)
  {
    switch (target)
    {
      case Node n   -> n.addEventFilter(type, handler);
      case Scene s  -> s.addEventFilter(type, handler);
      case Window w -> w.addEventFilter(type, handler);

      default       -> throw new IllegalArgumentException("Unsupported target: " + target.getClass());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <E extends Event> void removeFilterInternal(Object target, EventType<E> type, EventHandler<? super E> handler)
  {
    switch (target)
    {
      case Node n   -> n.removeEventFilter(type, handler);
      case Scene s  -> s.removeEventFilter(type, handler);
      case Window w -> w.removeEventFilter(type, handler);

      default       -> throw new IllegalArgumentException("Unsupported target: " + target.getClass());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private static <E extends Event> EventHandler<? super Event> cast(EventHandler<? super E> h)
  {
    return (EventHandler<? super Event>) h;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
