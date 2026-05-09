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

package org.hypernomicon;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;

import org.json.simple.parser.ParseException;

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

public final class DatabaseRegistry
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DatabaseRegistry() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  private static final String DATABASES_FILENAME = "databases.json";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void ensureAppDataDirExists()
  {
    FilePath dir = appDataDir();
    if (dir.exists() == false)
    {
      try { dir.createDirectories(); }
      catch (IOException e) { logThrowable(e); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Read and parse the JSON file at {@code filePath}. A missing file is not
   * an error (first-run case) and returns an empty {@link JsonObj}. Any other
   * failure (I/O error or unparseable content) propagates as {@link IOException}
   * so callers can abort destructive operations rather than silently treating
   * the registry as empty.
   */
  private static JsonObj loadJsonFile(FilePath filePath) throws IOException
  {
    if (filePath.exists() == false) return new JsonObj();

    try (Reader reader = new InputStreamReader(new FileInputStream(filePath.toFile()), StandardCharsets.UTF_8))
    {
      return JsonObj.parseJsonObj(reader);
    }
    catch (ParseException e)
    {
      throw new IOException("Unable to parse registry file: " + e.getMessage(), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Load all database entries from {@code databases.json}. Propagates
   * {@link IOException} from {@link #loadJsonFile}; callers should surface
   * the failure and abort destructive operations.
   */
  private static Map<String, FilePath> load() throws IOException
  {
    Map<String, FilePath> databases = new LinkedHashMap<>();

    JsonArray arr = loadJsonFile(appDataDir().resolve(DATABASES_FILENAME)).getArray("databases");
    if (arr != null)
    {
      for (int ndx = 0; ndx < arr.size(); ndx++)
      {
        JsonObj entryObj = arr.getObj(ndx);
        String dbID = entryObj.getStrSafe("dbID");

        if (strNullOrBlank(dbID) == false)
          databases.put(dbID, FilePath.of(entryObj.getStrSafe("rootPath")));
      }
    }

    return databases;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void save(Map<String, FilePath> databases)
  {
    JsonObj rootObj = new JsonObj();
    JsonArray arr = new JsonArray();

    databases.forEach((dbID, rootPath) ->
    {
      if (FilePath.isEmpty(rootPath)) return;

      JsonObj entryObj = new JsonObj();
      entryObj.put("dbID", dbID);
      entryObj.put("rootPath", rootPath.toString());
      arr.add(entryObj);
    });

    rootObj.put("databases", arr);

    ensureAppDataDirExists();

    try
    {
      appDataDir().resolve(DATABASES_FILENAME).saveCharSequenceAtomically(rootObj.toString(), StandardCharsets.UTF_8);
    }
    catch (IOException e)
    {
      errorPopup("Unable to write to registry file: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Look up the root path of a known database by its DBID.
   * @param dbID The database identifier to look up
   * @return The registered root path, or {@code null} if no entry exists
   *         (or the registry could not be read; an error popup is shown in
   *         that case)
   */
  public static FilePath findByDBID(String dbID)
  {
    if (strNullOrBlank(dbID)) return null;

    try { return load().get(dbID); }
    catch (IOException e)
    {
      errorPopup("Unable to read registry file: " + getThrowableMessage(e));
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Register or update a database entry in the registry.
   * @param dbID The database identifier
   * @param rootPath The root path of the database
   */
  public static void registerDatabase(String dbID, FilePath rootPath)
  {
    if (strNullOrBlank(dbID)) return;

    Map<String, FilePath> databases;

    try { databases = load(); }
    catch (IOException e)
    {
      errorPopup("Unable to read registry file: " + getThrowableMessage(e));
      return;
    }

    databases.put(dbID, rootPath);

    save(databases);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Resolve the index directory for a database, creating it if needed.
   * @param dbID The database identifier
   * @return The index directory path, or null if dbID is blank
   */
  public static FilePath resolveIndexDir(String dbID)
  {
    if (strNullOrBlank(dbID)) return null;

    FilePath indexDir = appDataDir().resolve("indexes").resolve(dbID);

    if (indexDir.exists() == false)
    {
      try { indexDir.createDirectories(); }
      catch (IOException e) { logThrowable(e); }
    }

    return indexDir;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Garbage-collect the registry: remove entries whose databases no longer
   * exist on disk (i.e., {@code XML/Settings.xml} is missing at the registered
   * root) and delete any index directories whose dbID is not in the (post-
   * pruning) registry. Skips the currently loaded database in both passes.
   *
   * @param currentDBID the DBID of the currently loaded database (never removed)
   */
  public static void garbageCollect(String currentDBID)
  {
    Map<String, FilePath> databases;

    try { databases = load(); }
    catch (IOException e)
    {
      errorPopup("Unable to read registry file; cleanup aborted: " + getThrowableMessage(e));
      return;
    }

    // First pass: prune stale registry entries (DBs whose Settings.xml no longer exists).

    Set<String> staleDbIDs = new HashSet<>();
    databases.forEach((dbID, rootPath) ->
    {
      if (dbID.equals(currentDBID)) return;

      if (FilePath.isEmpty(rootPath) || (rootPath.resolve("XML").resolve("Settings.xml").exists() == false))
        staleDbIDs.add(dbID);
    });

    boolean dirty = staleDbIDs.isEmpty() == false;
    staleDbIDs.forEach(databases::remove);

    // Second pass: delete index directories whose dbID is no longer in the registry.
    // Both freshly-pruned entries and pre-existing orphans land in this set.

    FilePath indexesDir = appDataDir().resolve("indexes");

    if (indexesDir.exists())
    {
      try (var stream = Files.list(indexesDir.toPath()))
      {
        stream.filter(Files::isDirectory).map(FilePath::of).forEach(dir ->
        {
          String dirName = dir.getNameOnly().toString();
          if (dirName.equals(currentDBID) || databases.containsKey(dirName)) return;

          FileDeletion.ofDirWithContents(dir).nonInteractiveLogErrors().execute();
          System.out.println("Cleaned stale index directory: " + dir);
        });
      }
      catch (IOException e) { logThrowable(e); }
    }

    if (dirty) save(databases);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
