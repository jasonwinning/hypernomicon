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

package org.hypernomicon.fts;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.util.Version;
import org.apache.tika.Tika;

import org.hypernomicon.App;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Captures the indexing configuration at a point in time so the indexer can
 * detect when that configuration has changed.
 * <p>
 * The metadata snapshot records the manifest its entries were built under. On
 * startup that record is compared field by field with the current configuration,
 * and the result is a {@link StaleScope}: a change to a setting every entry
 * depends on (schema version, analyzer, Lucene version) makes every entry stale,
 * while a new version of one text extractor makes only the entries that extractor
 * produced stale. Stale files are re-extracted in place while the existing index
 * remains searchable.
 * <p>
 * The list of indexable extensions is recorded but never compared: which other
 * file types are indexable has no bearing on the text already extracted from a
 * file. Files of a newly indexable type are picked up by the initial build and
 * the consistency check like any other new file.
 * <p>
 * A copy is also stored as {@code index-manifest.json} alongside the Lucene index
 * and metadata. It is read back only to interpret a snapshot from a version that
 * stamped a single hash of the whole configuration instead of the manifest itself
 * (see {@link #forLegacySnapshot}).
 */
final class IndexManifest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Which entries built under another configuration are due for re-extraction.
   *
   * @param everything     every entry, including one that no extractor handled
   * @param extractorKinds otherwise, the entries these extractors handled
   */
  record StaleScope(boolean everything, Set<ExtractorKind> extractorKinds)
  {
    boolean includes(ExtractorKind kind) { return everything || ((kind != null) && extractorKinds.contains(kind)); }
    boolean isNothing()                  { return (everything == false) && extractorKinds.isEmpty(); }
  }

//---------------------------------------------------------------------------

  private static final int CURRENT_MANIFEST_FORMAT_VERSION = 2;

  /** The pdf.js version bundled with the releases (1.36 and 1.36.1, which share one index
   *  format) whose indexes are compatible with the current schema version but whose snapshots
   *  do not say which pdf.js version they were built with. Snapshots from older releases
   *  predate the current schema version, so all of their entries are stale whatever this says. */
  private static final String LEGACY_PDFJS_VERSION = "6.3.289";

  private static final Pattern PDFJS_VERSION_PATTERN = Pattern.compile("pdfjsVersion\\s*=\\s*(\\S+)");

  private static final String PDFJS_LIBRARY_RESOURCE = "resources/pdfjs/build/pdf.mjs";  // relative to the org.hypernomicon package

  private final int manifestFormatVersion, indexSchemaVersion;
  private final String analyzerClass, luceneVersion;
  private final Map<ExtractorKind, String> extractorVersions;
  private final List<String> indexableExtensions;

//---------------------------------------------------------------------------

  private IndexManifest(int manifestFormatVersion, int indexSchemaVersion, String analyzerClass,
                        List<String> indexableExtensions, String luceneVersion, Map<ExtractorKind, String> extractorVersions)
  {
    this.manifestFormatVersion = manifestFormatVersion;
    this.indexSchemaVersion = indexSchemaVersion;
    this.analyzerClass = analyzerClass;
    this.indexableExtensions = indexableExtensions;
    this.luceneVersion = luceneVersion;
    this.extractorVersions = extractorVersions;
  }

//---------------------------------------------------------------------------

  String extractorVersion(ExtractorKind kind)          { return extractorVersions.get(kind); }

  private static String versionKey(ExtractorKind kind) { return kind.jsonName() + "Version"; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Build a manifest from the current live configuration.
   */
  static IndexManifest computeCurrent(Set<String> extensions, int schemaVersion)
  {
    List<String> sortedExts = new ArrayList<>(extensions);
    Collections.sort(sortedExts);

    String analyzer = StandardTokenizer.class.getName() + "+LowerCaseFilter+ASCIIFoldingFilter",
           lucene   = Version.LATEST.toString();

    Map<ExtractorKind, String> versions = new EnumMap<>(ExtractorKind.class);

    versions.put(ExtractorKind.PDFJS, detectPdfjsVersion());
    versions.put(ExtractorKind.TIKA , detectTikaVersion ());

    return new IndexManifest(CURRENT_MANIFEST_FORMAT_VERSION, schemaVersion, analyzer, sortedExts, lucene, versions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Returns a copy of this manifest that names a different version of one extractor. */
  IndexManifest withExtractorVersion(ExtractorKind kind, String version)
  {
    Map<ExtractorKind, String> versions = new EnumMap<>(extractorVersions);
    versions.put(kind, version);

    return new IndexManifest(manifestFormatVersion, indexSchemaVersion, analyzerClass, indexableExtensions, luceneVersion, versions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns which entries built under the other manifest are due for re-extraction
   * under this one.
   *
   * @param builtUnder the manifest the entries were built under, or {@code null}
   *                   if that is not known, in which case every entry is stale
   */
  StaleScope staleScopeSince(IndexManifest builtUnder)
  {
    if ((builtUnder == null)
        || (indexSchemaVersion != builtUnder.indexSchemaVersion)
        || (analyzerClass.equals(builtUnder.analyzerClass) == false)
        || (luceneVersion.equals(builtUnder.luceneVersion) == false))
      return new StaleScope(true, Set.of());

    Set<ExtractorKind> kinds = EnumSet.noneOf(ExtractorKind.class);

    for (ExtractorKind kind : ExtractorKind.values())
      if (extractorVersion(kind).equals(builtUnder.extractorVersion(kind)) == false)
        kinds.add(kind);

    return new StaleScope(false, kinds);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Works out what a snapshot from before manifests were recorded in the snapshot
   * was built under. Such a snapshot carries only a hash of the configuration, but
   * the version that wrote it also kept the manifest file up to date, with the
   * same fields in plain form. If the stored manifest's fields hash to the
   * snapshot's value, they are the configuration the snapshot was built under.
   * <p>
   * Comparing those fields, instead of accepting or rejecting the hash as a whole,
   * is what lets an index carry over when only the list of indexable extensions or
   * the version of one extractor has changed since.
   *
   * @return the manifest the snapshot was built under, or {@code null} if it cannot be established
   */
  static IndexManifest forLegacySnapshot(IndexManifest storedManifest, String snapshotConfigHash)
  {
    if ((storedManifest == null) || strNullOrBlank(snapshotConfigHash)) return null;

    if (storedManifest.legacyConfigHash().equals(snapshotConfigHash) == false) return null;

    return storedManifest.withExtractorVersion(ExtractorKind.PDFJS, LEGACY_PDFJS_VERSION);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Load a manifest from disk. Returns {@code null} if the file is missing or corrupt.
   */
  static IndexManifest loadFrom(FilePath path)
  {
    if ((path == null) || (path.exists() == false)) return null;

    try
    {
      return fromJson(JsonObj.parseJsonObj(Files.readString(path.toPath(), StandardCharsets.UTF_8)));
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: failed to load manifest: " + getThrowableMessage(e));
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static IndexManifest fromJson(JsonObj obj)
  {
    int manifestFmtVer = (int) obj.getLong("manifestFormatVersion", 0L),
        schemaVer      = (int) obj.getLong("indexSchemaVersion", 0L);

    String analyzer = obj.getStrSafe("analyzerClass"),
           lucene   = obj.getStrSafe("luceneVersion");

    Map<ExtractorKind, String> versions = new EnumMap<>(ExtractorKind.class);

    for (ExtractorKind kind : ExtractorKind.values())
      versions.put(kind, obj.getStrSafe(versionKey(kind)));

    JsonArray extArr = obj.getArray("indexableExtensions");
    List<String> exts = new ArrayList<>();

    if (extArr != null)
      extArr.strStream().forEach(exts::add);

    return new IndexManifest(manifestFmtVer, schemaVer, analyzer, exts, lucene, versions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  JsonObj toJson()
  {
    JsonObj obj = new JsonObj();

    obj.put("manifestFormatVersion", (long) manifestFormatVersion);
    obj.put("indexSchemaVersion", (long) indexSchemaVersion);
    obj.put("analyzerClass", analyzerClass);

    JsonArray extArr = new JsonArray();
    for (String ext : indexableExtensions)
      extArr.add(ext);

    obj.put("indexableExtensions", extArr);
    obj.put("luceneVersion", luceneVersion);

    for (ExtractorKind kind : ExtractorKind.values())
      obj.put(versionKey(kind), extractorVersion(kind));

    return obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Save this manifest to disk atomically.
   */
  void saveTo(FilePath path) throws IOException
  {
    path.saveCharSequenceAtomically(toJson().toString(), StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a human-readable description of which of the compared fields differ
   * between this manifest and another. Used for diagnostic logging when a
   * configuration change is detected.
   */
  String describeDifferences(IndexManifest other)
  {
    if (other == null) return "no record of the configuration the index was built under";

    List<String> diffs = new ArrayList<>();

    if (indexSchemaVersion != other.indexSchemaVersion)
      diffs.add("indexSchemaVersion: " + other.indexSchemaVersion + " -> " + indexSchemaVersion);

    if (analyzerClass.equals(other.analyzerClass) == false)
      diffs.add("analyzerClass: " + other.analyzerClass + " -> " + analyzerClass);

    if (luceneVersion.equals(other.luceneVersion) == false)
      diffs.add("luceneVersion: " + other.luceneVersion + " -> " + luceneVersion);

    for (ExtractorKind kind : ExtractorKind.values())
      if (extractorVersion(kind).equals(other.extractorVersion(kind)) == false)
        diffs.add(versionKey(kind) + ": " + other.extractorVersion(kind) + " -> " + extractorVersion(kind));

    return String.join("; ", diffs);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The hash that versions before manifest format 2 stamped on the metadata
   * snapshot. The formula is frozen: it has to reproduce what those versions
   * wrote, so it must not pick up fields added since.
   */
  private String legacyConfigHash()
  {
    String canonical = "indexSchemaVersion=" + indexSchemaVersion
                     + "|analyzerClass=" + analyzerClass
                     + "|indexableExtensions=" + String.join(",", indexableExtensions)
                     + "|luceneVersion=" + luceneVersion
                     + "|tikaVersion=" + extractorVersion(ExtractorKind.TIKA);
    try
    {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      digest.update(canonical.getBytes(StandardCharsets.UTF_8));
      return digestHexStr(digest);
    }
    catch (NoSuchAlgorithmException e)
    {
      throw new RuntimeException("SHA-256 not available", e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reads the pdf.js version from the header comment that every pdf.js
   * distribution file carries near its top.
   */
  private static String detectPdfjsVersion()
  {
    try (InputStream is = App.class.getResourceAsStream(PDFJS_LIBRARY_RESOURCE))
    {
      if (is != null)
      {
        BufferedReader reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8));

        for (int lineNdx = 0; lineNdx < 100; lineNdx++)
        {
          String line = reader.readLine();
          if (line == null) break;

          Matcher matcher = PDFJS_VERSION_PATTERN.matcher(line);
          if (matcher.find())
            return matcher.group(1);
        }
      }
    }
    catch (IOException e) { /* fall through */ }

    System.out.println("Full-text indexer: WARNING: unable to detect pdf.js version; using \"unknown\"");
    return "unknown";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Detect the Tika version at runtime. Tries the jar manifest first, then
   * falls back to reading Maven's {@code pom.properties} from the classpath.
   */
  private static String detectTikaVersion()
  {
    // Try standard jar manifest attribute

    Package pkg = Tika.class.getPackage();

    if (pkg != null)
    {
      String ver = pkg.getImplementationVersion();
      if (strNotNullOrBlank(ver))
        return ver;
    }

    // Fallback: read Maven pom.properties from classpath

    try (InputStream is = Tika.class.getClassLoader().getResourceAsStream("META-INF/maven/org.apache.tika/tika-core/pom.properties"))
    {
      if (is != null)
      {
        Properties props = new Properties();
        props.load(is);

        String ver = props.getProperty("version");
        if (strNotNullOrBlank(ver))
          return ver;
      }
    }
    catch (IOException | SecurityException e) { /* fall through */ }

    System.out.println("Full-text indexer: WARNING: unable to detect Tika version; using \"unknown\"");
    return "unknown";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
