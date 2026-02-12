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

package org.hypernomicon.util.file;

import static org.hypernomicon.Const.FilenameRulesPrefKey.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.prefs.Preferences;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.file.deletion.FileDeletion;

import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.lang.UProperty;
import com.ibm.icu.text.Normalizer2;

//---------------------------------------------------------------------------

/**
 * Filesystem filename equivalence rules detected by probing the actual filesystem.
 * <p>
 * Different filesystems have different rules for when two filenames are considered equivalent:
 * <ul>
 *   <li>Case sensitivity (APFS can be either; NTFS is case-insensitive)</li>
 *   <li>Unicode composition form (APFS treats NFC and NFD as equivalent)</li>
 *   <li>Trailing dots/spaces (Windows strips them)</li>
 *   <li>Default ignorable code points (some filesystems ignore ZWJ, etc.)</li>
 * </ul>
 * <p>
 * This record detects actual filesystem behavior by creating temporary files and checking
 * whether the filesystem considers different filenames equivalent.
 * <p>
 * Usage:
 * <ul>
 *   <li>Call {@link #initialize(Preferences prefs)} at application startup to load saved rules or probe temp directory</li>
 *   <li>Call {@link #updateForPath(FilePath)} when loading a database to detect rules for that volume</li>
 *   <li>Use {@link #current()} to get the current filename rules for FilenameMap</li>
 * </ul>
 *
 * @param caseInsensitive true if the filesystem treats {@code File.txt} and {@code FILE.TXT} as the same file
 *        (NTFS, APFS default). False for case-sensitive filesystems (ext4, case-sensitive APFS).
 * @param unicodeCompInsensitive true if the filesystem treats different Unicode composition forms as equivalent,
 *        so precomposed {@code é} (U+00E9, NFC) and decomposed {@code e} + combining accent (U+0065 U+0301, NFD)
 *        refer to the same file. True for APFS; false for NTFS and most Linux filesystems.
 * @param trimsTrailingDotsAndSpaces true if the filesystem strips trailing dots and spaces from filenames,
 *        making {@code file.txt.} equivalent to {@code file.txt}. True for Windows/NTFS.
 * @param ignoresDefaultIgnorables true if the filesystem ignores Unicode default ignorable code points
 *        (zero-width joiners, variation selectors, etc.) when comparing filenames.
 * @param caseFoldingMode the Unicode case folding mode used for case-insensitive comparison.
 *        {@link CaseFoldingMode#SIMPLE} keeps ß distinct from ss; {@link CaseFoldingMode#FULL} treats them as equivalent.
 */
public record FilenameRules(boolean caseInsensitive, boolean unicodeCompInsensitive, boolean trimsTrailingDotsAndSpaces,
                            boolean ignoresDefaultIgnorables, CaseFoldingMode caseFoldingMode)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Case folding strategy. */
  public enum CaseFoldingMode
  {
    /** Per-codepoint folding, no expansions (ß stays ß). Safer default. */
    SIMPLE("simple"),
    /** Full Unicode folding with expansions (ß → ss). */
    FULL("full");

    CaseFoldingMode(String prefVal) { this.prefVal = prefVal; }

    private final String prefVal;

    static CaseFoldingMode fromPrefVal(String prefVal)
    {
      return Arrays.stream(values()).filter(enumVal -> prefVal.equals(enumVal.prefVal)).findFirst().orElse(SIMPLE);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Heuristic rules for Windows/NTFS: case-insensitive, trims trailing dots/spaces. */
  static final FilenameRules WINDOWS_HEURISTIC = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);

  /** Heuristic rules for macOS/APFS: case-insensitive, Unicode composition insensitive, full case folding. */
  static final FilenameRules MAC_HEURISTIC = new FilenameRules(true, true, false, false, CaseFoldingMode.FULL);

  /** Heuristic rules for Linux/ext4: case-sensitive, no special handling. */
  static final FilenameRules LINUX_HEURISTIC = new FilenameRules(false, false, false, false, CaseFoldingMode.SIMPLE);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static volatile FilenameRules currentRules = null;
  private static volatile boolean initialized = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Heuristic rules based on OS. Prefer {@link #detect(Path)} for accuracy. */
  static FilenameRules heuristicForCurrentOS()
  {
    return switch (CURRENT_OS)
    {
      case WINDOWS -> WINDOWS_HEURISTIC;
      case MAC     -> MAC_HEURISTIC;
      default      -> LINUX_HEURISTIC;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the current filename rules.
   * <p>
   * Must be called after {@link #initialize(Preferences prefs)} has been called at application startup
   * or called from a unit test.
   *
   * @return the current rules
   * @throws IllegalStateException if called before initialization outside a unit test context
   */
  public static FilenameRules current()
  {
    if (currentRules != null)
      return currentRules;

    if (initialized == false)
    {
      if (isUnitTestThread())
      {
        currentRules = heuristicForCurrentOS();
        return currentRules;
      }

      throw new IllegalStateException("FilenameRules.current() called before FilenameRules.initialize().");
    }

    // initialized is true but currentRules is null; should not happen, but safe fallback
    currentRules = heuristicForCurrentOS();
    return currentRules;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Initializes the filename rules from saved preferences or by probing the temp directory.
   * <p>
   * This method should be called once at application startup.
   */
  public static void initialize(Preferences prefs)
  {
    if (prefs.getBoolean(INITIALIZED, false))
    {
      currentRules = fromPrefs(prefs);
    }
    else
    {
      try
      {
        currentRules = detect(tempDir());
        currentRules.saveTo(prefs);
      }
      catch (IOException e)
      {
        logThrowable(e);

        // Fall back to heuristic if probing fails
        currentRules = heuristicForCurrentOS();
      }
    }

    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Updates the filename rules by probing the specified path's filesystem.
   * <p>
   * This method should be called when loading a database to detect rules for that volume,
   * which may differ from the temp directory's filesystem.
   *
   * @param filePath a path on the filesystem to probe (typically the database root)
   * @throws IOException if probing fails
   * @throws IllegalArgumentException if filePath is empty or has no parent directory
   */
  public static void updateForPath(FilePath filePath) throws IOException
  {
    if (FilePath.isEmpty(filePath))
      throw new IllegalArgumentException("filePath cannot be empty");

    FilePath dir = filePath.isDirectory() ? filePath : filePath.getParent();

    if (FilePath.isEmpty(dir))
      throw new IllegalArgumentException("filePath has no parent directory");

    if (dir.exists() == false)
      throw new IllegalArgumentException("Directory does not exist: " + dir);

    currentRules = detect(dir);
    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Probe filesystem to detect actual rules. Performs I/O.
   *
   * @param directory a writable directory on the filesystem to probe
   * @return the detected rules
   * @throws IOException if the directory is not writable or probing fails
   */
  static FilenameRules detect(FilePath directory) throws IOException
  {
    Objects.requireNonNull(directory, "directory");

    return detect(directory.toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String PROBE_DIR_PREFIX = "filenamerules-probe-";

  /**
   * Probe filesystem to detect actual rules. Performs I/O.
   *
   * @param directory a writable directory on the filesystem to probe
   * @return the detected rules
   * @throws IOException if the directory is not writable or probing fails
   */
  static FilenameRules detect(Path directory) throws IOException
  {
    Objects.requireNonNull(directory, "directory");

    Path dir = directory.toAbsolutePath().normalize();

    if (Files.isDirectory(dir) == false)
      throw new NotDirectoryException(dir.toString());

    if (Files.isWritable(dir) == false)
      throw new IOException("Directory not writable: " + dir);

    cleanupLeftoverProbeDirectories(dir);

    Path sandbox = Files.createTempDirectory(dir, PROBE_DIR_PREFIX);

    try
    {
      String token = UUID.randomUUID().toString().replace("-", "");

      boolean caseInsensitive = (fileSystemTreatsAsDistinct(sandbox, "case_" + token + "_abc", "CASE_" + token + "_ABC") == false),

              unicodeCompInsensitive = (fileSystemTreatsAsDistinct(sandbox, "norm_" + token + "_\u00E9", "norm_" + token + "_e\u0301") == false),

              trimsTrailingDots   = (fileSystemTreatsAsDistinct(sandbox, "trim_" + token + "_a", "trim_" + token + "_a.") == false),
              trimsTrailingSpaces = (fileSystemTreatsAsDistinct(sandbox, "trim_" + token + "_b", "trim_" + token + "_b ") == false),

              // Use OR: if filesystem trims either, we trim both during normalization. This may cause
              // false collision warnings on filesystems that trim only one (e.g., exFAT trims dots
              // but not spaces), but errs on the side of safety (no missed collisions).

              trimsTrailing     = trimsTrailingDots || trimsTrailingSpaces,

              ignoresIgnorables = caseInsensitive && (fileSystemTreatsAsDistinct(sandbox, "ign_" + token + "_ab", "ign_" + token + "_a\u200Db") == false);

      CaseFoldingMode foldMode = CaseFoldingMode.SIMPLE;

      if (caseInsensitive && (fileSystemTreatsAsDistinct(sandbox, "fold_" + token + "_\u00DF", "fold_" + token + "_ss") == false))
        foldMode = CaseFoldingMode.FULL;

      return new FilenameRules(caseInsensitive, unicodeCompInsensitive, trimsTrailing, ignoresIgnorables, foldMode);
    }
    finally
    {
      cleanupLeftoverProbeDirectory(sandbox);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void cleanupLeftoverProbeDirectory(Path path)
  {
    FilePath probeDir = new FilePath(path);

    HyperThread cleanupThread = new HyperThread("ProbeCleanup", () -> FileDeletion.ofDirWithContents(probeDir).nonInteractiveFailureOK().execute());

    cleanupThread.setDaemon(true);
    cleanupThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void cleanupLeftoverProbeDirectories(Path dir)
  {
    Set<FilePath> probeDirs = new LinkedHashSet<>();

    try (var stream = Files.newDirectoryStream(dir, PROBE_DIR_PREFIX + '*'))
    {
      for (Path path : stream)
      {
        if (Files.isDirectory(path))
          probeDirs.add(new FilePath(path));
      }
    }
    catch (IOException e)
    {
      // Best-effort cleanup; ignore failures
      return;
    }

    if (probeDirs.isEmpty())
      return;

    HyperThread cleanupThread = new HyperThread("ProbeCleanup", () -> FileDeletion.ofDirsWithContents(probeDirs).nonInteractiveFailureOK().execute());

    cleanupThread.setDaemon(true);
    cleanupThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Loads rules from preferences. */
  private static FilenameRules fromPrefs(Preferences prefs)
  {
    boolean caseInsensitive        = prefs.getBoolean(CASE_INSENSITIVE        , false),
            unicodeCompInsensitive = prefs.getBoolean(UNICODE_COMP_INSENSITIVE, false),
            trimsTrailing          = prefs.getBoolean(TRIMS_TRAILING          , false),
            ignoresIgnorables      = prefs.getBoolean(IGNORES_IGNORABLES      , false);

    CaseFoldingMode foldMode = CaseFoldingMode.fromPrefVal(prefs.get(CASE_FOLDING_MODE, CaseFoldingMode.SIMPLE.prefVal));

    return new FilenameRules(caseInsensitive, unicodeCompInsensitive, trimsTrailing, ignoresIgnorables, foldMode);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Saves these rules to preferences. */
  private void saveTo(Preferences prefs)
  {
    prefs.putBoolean(CASE_INSENSITIVE        , caseInsensitive);
    prefs.putBoolean(UNICODE_COMP_INSENSITIVE, unicodeCompInsensitive);
    prefs.putBoolean(TRIMS_TRAILING          , trimsTrailingDotsAndSpaces);
    prefs.putBoolean(IGNORES_IGNORABLES      , ignoresDefaultIgnorables);
    prefs.put       (CASE_FOLDING_MODE       , caseFoldingMode.prefVal);
    prefs.putBoolean(INITIALIZED             , true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Normalizer2 NFC = Normalizer2.getNFCInstance();

  /**
   * Normalizes a filename according to these rules.
   *
   * @param filename the filename to normalize
   * @return the normalized filename
   * @throws IllegalArgumentException if the filename is invalid (contains path separators, NUL, or is "." or "..")
   */
  public String normalize(String filename)
  {
    if (filename == null)
      return null;

    validateFilename(filename);

    String s = filename;

    if (trimsTrailingDotsAndSpaces)
      s = trimTrailingDotsAndSpaces(s);

    if (s.isEmpty())
      return s;

    if (isAscii(s))
    {
      // ASCII path: only case folding needed (no NFC normalization, no default ignorables)

      return caseInsensitive ? toLowerAsciiStr(s) : s;
    }

    // Non-ASCII path: may need case folding, NFC normalization, and/or ignorable stripping

    if (caseInsensitive)
    {
      if (ignoresDefaultIgnorables)
        s = stripDefaultIgnorables(s);

      s = caseFoldingMode == CaseFoldingMode.FULL ? UCharacter.foldCase(s, true) : foldCaseSimple(s);
    }

    // Single NFC normalization at end (handles both original non-NFC and any denormalization from case folding)

    if (unicodeCompInsensitive && (NFC.isNormalized(s) == false))
      s = NFC.normalize(s);

    return s;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void validateFilename(String filename)
  {
    // Empty string is allowed (represents root directory)
    if (filename.isEmpty())
      return;

    // Reject "." and ".."
    if (".".equals(filename) || "..".equals(filename))
      throw new IllegalArgumentException("Invalid filename: " + filename);

    // Reject filenames containing NUL
    if (filename.indexOf('\0') >= 0)
      throw new IllegalArgumentException("Filename contains NUL character");

    // Reject filenames containing path separators
    if (filename.indexOf('/') >= 0)
      throw new IllegalArgumentException("Filename contains path separator: " + filename);

    if (IS_OS_WINDOWS && (filename.indexOf('\\') >= 0))
      throw new IllegalArgumentException("Filename contains path separator: " + filename);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String trimTrailingDotsAndSpaces(String s)
  {
    int end = s.length();

    while (end > 0)
    {
      char lastChar = s.charAt(end - 1);

      if ((lastChar != '.') && (lastChar != ' '))
        break;

      end--;
    }

    return (end == s.length()) ? s : s.substring(0, end);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String foldCaseSimple(String s)
  {
    StringBuilder sb = null;

    for (int ndx = 0; ndx < s.length(); )
    {
      int cp = s.codePointAt(ndx),
          folded = UCharacter.foldCase(cp, true);

      if (folded != cp)
      {
        if (sb == null)
        {
          sb = new StringBuilder(s.length());
          sb.append(s, 0, ndx);
        }

        sb.appendCodePoint(folded);
      }
      else if (sb != null)
      {
        sb.appendCodePoint(cp);
      }

      ndx += Character.charCount(cp);
    }

    return (sb == null) ? s : sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String stripDefaultIgnorables(String s)
  {
    StringBuilder sb = null;

    for (int ndx = 0; ndx < s.length(); )
    {
      int cp = s.codePointAt(ndx);

      if (UCharacter.hasBinaryProperty(cp, UProperty.DEFAULT_IGNORABLE_CODE_POINT))
      {
        if (sb == null)
        {
          sb = new StringBuilder(s.length());
          sb.append(s, 0, ndx);
        }
      }
      else if (sb != null)
      {
        sb.appendCodePoint(cp);
      }

      ndx += Character.charCount(cp);
    }

    return (sb == null) ? s : sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Tests whether the filesystem treats two filenames as distinct.
   *
   * @param dir the directory to create test files in
   * @param baseFilename the first filename to create
   * @param variantFilename the variant filename to test against the base
   * @return {@code true} if the filesystem treats them as distinct files,
   *         {@code false} if it treats them as equivalent (same file)
   * @throws IOException if the test could not be performed
   */
  private static boolean fileSystemTreatsAsDistinct(Path dir, String baseFilename, String variantFilename) throws IOException
  {
    Path basePath = dir.resolve(baseFilename),
         variantPath;

    try
    {
      variantPath = dir.resolve(variantFilename);
    }
    catch (InvalidPathException e)
    {
      // If the variant filename can't even be parsed as a path (e.g., trailing space on Windows),
      // the filesystem effectively treats the variant as equivalent to the normalized form

      return false;
    }

    boolean createdBase = false,
            createdVariant = false;

    try
    {
      Files.createFile(basePath);
      createdBase = true;

      try
      {
        Files.createFile(variantPath);
        createdVariant = true;
        return true;
      }
      catch (FileAlreadyExistsException e)
      {
        return false;
      }
      catch (IOException e)
      {
        // Variant file creation failed for another reason (e.g., filesystem rejects the characters).
        // Conservatively treat as distinct; if the variant filename can't exist, it's effectively
        // distinct from the base filename. This handles FAT32 rejecting Unicode, exFAT rejecting
        // trailing spaces, NAS devices rejecting ZWJ, etc.

        return true;
      }
    }
    finally
    {
      if (createdBase)
        FileDeletion.ofFile(new FilePath(basePath)).nonInteractiveFailureOK().execute();

      if (createdVariant)
        FileDeletion.ofFile(new FilePath(variantPath)).nonInteractiveFailureOK().execute();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
