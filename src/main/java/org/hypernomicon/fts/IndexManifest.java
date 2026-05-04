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

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.util.Version;
import org.apache.tika.Tika;

import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Captures the indexing configuration at a point in time so the indexer can
 * detect when a rebuild is needed. Stored as {@code index-manifest.json}
 * alongside the Lucene index and metadata.
 * <p>
 * The {@link #configHash} is a SHA-256 digest of all indexing-affecting fields.
 * On startup, the current config is compared to the stored manifest; a mismatch
 * triggers a clean wipe-and-rebuild.
 */
final class IndexManifest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int CURRENT_MANIFEST_FORMAT_VERSION = 1;

  private final int manifestFormatVersion, indexSchemaVersion;
  private final String analyzerClass, luceneVersion, tikaVersion, configHash;
  private final List<String> indexableExtensions;

//---------------------------------------------------------------------------

  private IndexManifest(int manifestFormatVersion, int indexSchemaVersion, String analyzerClass,
                        List<String> indexableExtensions, String luceneVersion, String tikaVersion,
                        String configHash)
  {
    this.manifestFormatVersion = manifestFormatVersion;
    this.indexSchemaVersion = indexSchemaVersion;
    this.analyzerClass = analyzerClass;
    this.indexableExtensions = indexableExtensions;
    this.luceneVersion = luceneVersion;
    this.tikaVersion = tikaVersion;
    this.configHash = configHash;
  }

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
           lucene   = Version.LATEST.toString(),
           tika     = detectTikaVersion();

    String hash = computeHash(schemaVersion, analyzer, sortedExts, lucene, tika);

    return new IndexManifest(CURRENT_MANIFEST_FORMAT_VERSION, schemaVersion, analyzer, sortedExts, lucene, tika, hash);
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
      String json = Files.readString(path.toPath(), StandardCharsets.UTF_8);
      JsonObj obj = JsonObj.parseJsonObj(json);

      int manifestFmtVer = (int) obj.getLong("manifestFormatVersion", 0L),
          schemaVer      = (int) obj.getLong("indexSchemaVersion", 0L);

      String analyzer = obj.getStrSafe("analyzerClass"),
             lucene   = obj.getStrSafe("luceneVersion"),
             tika     = obj.getStrSafe("tikaVersion"),
             hash     = obj.getStrSafe("configHash");

      JsonArray extArr = obj.getArray("indexableExtensions");
      List<String> exts = new ArrayList<>();

      if (extArr != null)
        extArr.strStream().forEach(exts::add);

      return new IndexManifest(manifestFmtVer, schemaVer, analyzer, exts, lucene, tika, hash);
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: failed to load manifest: " + getThrowableMessage(e));
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Save this manifest to disk atomically.
   */
  void saveTo(FilePath path) throws IOException
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
    obj.put("tikaVersion", tikaVersion);
    obj.put("configHash", configHash);

    path.saveCharSequenceAtomically(obj.toString(), StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns {@code true} if the other manifest has the same config hash.
   */
  boolean matches(IndexManifest other)
  {
    return (other != null) && configHash.equals(other.configHash);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a human-readable description of which fields differ between this
   * manifest and another. Used for diagnostic logging when a rebuild is triggered.
   */
  String describeDifferences(IndexManifest other)
  {
    if (other == null) return "no previous manifest";

    List<String> diffs = new ArrayList<>();

    if (indexSchemaVersion != other.indexSchemaVersion)
      diffs.add("indexSchemaVersion: " + other.indexSchemaVersion + " -> " + indexSchemaVersion);

    if (analyzerClass.equals(other.analyzerClass) == false)
      diffs.add("analyzerClass: " + other.analyzerClass + " -> " + analyzerClass);

    if (indexableExtensions.equals(other.indexableExtensions) == false)
      diffs.add("indexableExtensions: " + other.indexableExtensions + " -> " + indexableExtensions);

    if (luceneVersion.equals(other.luceneVersion) == false)
      diffs.add("luceneVersion: " + other.luceneVersion + " -> " + luceneVersion);

    if (tikaVersion.equals(other.tikaVersion) == false)
      diffs.add("tikaVersion: " + other.tikaVersion + " -> " + tikaVersion);

    return diffs.isEmpty() ? "config hash mismatch (no field-level difference detected)" : String.join("; ", diffs);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String computeHash(int schemaVersion, String analyzer, Iterable<String> extensions,
                                    String lucene, String tika)
  {
    String canonical = "indexSchemaVersion=" + schemaVersion
                     + "|analyzerClass=" + analyzer
                     + "|indexableExtensions=" + String.join(",", extensions)
                     + "|luceneVersion=" + lucene
                     + "|tikaVersion=" + tika;
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
