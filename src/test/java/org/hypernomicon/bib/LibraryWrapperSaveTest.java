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

package org.hypernomicon.bib;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.util.json.JsonObj.JsonNodeType;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * The reference manager library file is written first in a save, and a failure to write it
 * must fail the whole save: the records files written after it would otherwise link works
 * to entries that were never saved, and the integrity manifest would certify that set.
 * <p>
 * A library file read at load and written back by a save must also hold the same data. The
 * file keeps what the server sent, including the fields Hypernomicon does not manage and the
 * spelling of every string, and a load followed by a save may change none of it.
 */
class LibraryWrapperSaveTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  private static final String TITLE_ALPHA_NFD = "Étude alpha",  // E followed by a combining acute accent (U+0301)
                              TITLE_ALPHA_NFC = "Étude alpha";    // the precomposed letter (U+00C9)

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither the linked library nor the temporary root for the next test class
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reopens the database with the given folder as its root and links an offline Zotero library,
   * which is how a newly linked database looks before its first save: no library file and no
   * library preferences.
   */
  private static ZoteroWrapper reopenAndLink(Path root) throws HyperDataException
  {
    db.closeAndOpen(FilePath.of(root));

    assertTrue(db.isOnline(), "Reopen failed");
    assertEquals("", db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""), "The template must not name a library");

    return db.linkBibLibrary(LibraryType.ltZotero, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aWriteFailureFailsTheSaveAndRecordsNothing(@TempDir Path root) throws Exception
  {
    ZoteroWrapper library = reopenAndLink(root);  // The XML subfolder does not exist, so the file cannot be written

    HyperDataException e = assertThrows(HyperDataException.class, () -> library.saveAllToPersistentStorage(null));

    assertTrue(e.getMessage().contains("saving bibliographic data"), e.getMessage());
    assertNotNull(e.getCause(), "The failure must carry the I/O exception");

    assertFalse(db.xmlPath().exists(), "A failed save must create nothing");
    assertNull(db.getBibChecksum(), "No checksum may be recorded for a file that was not written");
    assertEquals("", db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""), "No library preferences may be recorded for a file that was not written");
  }

//---------------------------------------------------------------------------

  @Test
  void aSuccessfulWriteRecordsTheChecksumAndThePreferences(@TempDir Path root) throws Exception
  {
    ZoteroWrapper library = reopenAndLink(root);

    db.xmlPath().createDirectories();

    assertDoesNotThrow(() -> library.saveAllToPersistentStorage(null));

    FilePath bibFilePath = db.xmlPath(BIB_FILE_NAME);

    assertTrue(bibFilePath.exists());
    assertFalse(db.xmlPath(BIB_FILE_NAME + ".tmp").exists(), "The temporary file must have been renamed to the final name");

    try (InputStream is = Files.newInputStream(bibFilePath.toPath()))
    {
      assertEquals(md5Hex(is), db.getBibChecksum(), "The recorded checksum must be the written file's");
    }

    assertEquals(LibraryType.ltZotero.descriptor, db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aZoteroLibraryFileRoundTripsUnchanged(@TempDir Path root) throws Exception
  {
    JsonObj libraryFile = zoteroLibraryFile();

    LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> library = linkToFile(root, LibraryType.ltZotero, libraryFile);

    assertEquals(3, library.getAllEntries().size(), "The trash holds entries too");
    assertEquals(1, library.getTrash().size());
    assertEquals(1, library.getKeyToColl().size());

    BibEntry<?, ?> alpha = library.getEntryByKey("alpha");

    assertEquals(TITLE_ALPHA_NFC, alpha.getStr(bfTitle), "Text is read composed");
    assertEquals(TITLE_ALPHA_NFD, alpha.exportStandaloneJsonObj(false).getObj("data").getStr("title"), "and kept as the server sent it");

    assertSameLibraryFile(libraryFile, saveTwice(library), "key");
  }

//---------------------------------------------------------------------------

  @Test
  void aMendeleyLibraryFileRoundTripsUnchanged(@TempDir Path root) throws Exception
  {
    JsonObj libraryFile = mendeleyLibraryFile();

    LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> library = linkToFile(root, LibraryType.ltMendeley, libraryFile);

    assertEquals(3, library.getAllEntries().size(), "The trash holds entries too");
    assertEquals(1, library.getTrash().size());
    assertEquals(1, library.getKeyToColl().size());

    BibEntry<?, ?> alpha = library.getEntryByKey("alpha");

    assertEquals(TITLE_ALPHA_NFC, alpha.getStr(bfTitle), "Text is read composed");
    assertEquals(TITLE_ALPHA_NFD, alpha.exportStandaloneJsonObj(false).getStr("title"), "and kept as the server sent it");

    assertSameLibraryFile(libraryFile, saveTwice(library), "id");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Writes the library file into a fresh database root and links an offline library of the given type that
   * reads it, as the real wrapper reads the file at load.
   */
  private static <T extends LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection>> T linkToFile(Path root, LibraryType libType, JsonObj libraryFile) throws Exception
  {
    db.closeAndOpen(FilePath.of(root));
    db.xmlPath().createDirectories();

    FilePath bibFilePath = db.xmlPath(BIB_FILE_NAME);

    bibFilePath.saveCharSequenceAtomically(libraryFile.toString(), XML_FILES_CHARSET);

    return db.linkBibLibrary(libType, "", bibFilePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Saves the library twice and checks that the second save wrote the same bytes as the first (the order of a
   * save's output depends on nothing but the data), then returns the saved file parsed.
   */
  private static JsonObj saveTwice(LibraryWrapper<?, ?> library) throws Exception
  {
    FilePath bibFilePath = db.xmlPath(BIB_FILE_NAME);

    library.saveAllToPersistentStorage(null);

    byte[] firstSave = Files.readAllBytes(bibFilePath.toPath());

    library.saveAllToPersistentStorage(null);

    assertArrayEquals(firstSave, Files.readAllBytes(bibFilePath.toPath()), "Saving unchanged data again must write the same bytes");

    return JsonObj.parseJsonObj(new String(firstSave, XML_FILES_CHARSET));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A Zotero library file as the application writes one: each entry carries the server's last version under
   * {@code backupItem}, deleted entries are under {@code trash}, and strings are as the server sent them, here
   * with one title decomposed into a letter and a combining mark.
   */
  private static JsonObj zoteroLibraryFile()
  {
    JsonObj libraryFile = new JsonObj();

    JsonArray items = libraryFile.getOrAddArray("items");

    items.add(zoteroItem("bravo", 7L, "Title bravo"));
    items.add(zoteroItem("alpha", 3L, TITLE_ALPHA_NFD));

    libraryFile.getOrAddArray("trash").add(zoteroItem("charlie", 5L, "Title charlie"));

    JsonObj collection = new JsonObj(), collectionData = new JsonObj();

    collectionData.put("key", "delta");
    collectionData.put("name", "Collection delta");
    collectionData.put("parentCollection", Boolean.FALSE);

    collection.put("key", "delta");
    collection.put("version", 2L);
    collection.put("data", collectionData);

    libraryFile.getOrAddArray("collections").add(collection);

    return libraryFile;
  }

//---------------------------------------------------------------------------

  private static JsonObj zoteroItem(String key, long version, String title)
  {
    JsonObj data = new JsonObj(), creator = new JsonObj();

    creator.put("creatorType", "author");
    creator.put("firstName", "Zoë");  // e followed by a combining diaeresis (U+0308)
    creator.put("lastName", "Alfa");

    data.put("key", key);
    data.put("version", version);
    data.put("itemType", "book");
    data.put("title", title);
    data.put("abstractNote", "Café " + key);  // A field Hypernomicon does not manage, decomposed like the title
    data.put("date", "2001");
    data.getOrAddArray("creators").add(creator);
    data.getOrAddArray("collections").add("delta");
    data.put("relations", new JsonObj());

    JsonObj item = new JsonObj();

    item.put("key", key);
    item.put("version", version);
    item.put("data", data);

    JsonObj backupItem = item.deepCopy();  // The server's version as last seen; it differs from the local one here, so the two must be kept apart

    backupItem.getObj("data").put("title", title + " as last synced");

    item.put("backupItem", backupItem);

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A Mendeley library file as the application writes one; see {@link #zoteroLibraryFile()}.
   */
  private static JsonObj mendeleyLibraryFile()
  {
    JsonObj libraryFile = new JsonObj();

    JsonArray documents = libraryFile.getOrAddArray("documents");

    documents.add(mendeleyDocument("bravo", "Title bravo"));
    documents.add(mendeleyDocument("alpha", TITLE_ALPHA_NFD));

    libraryFile.getOrAddArray("trash").add(mendeleyDocument("charlie", "Title charlie"));

    JsonObj folder = new JsonObj();

    folder.put("id", "delta");
    folder.put("name", "Folder delta");
    folder.put("last_modified", "2026-01-02T03:04:05.000Z");

    libraryFile.getOrAddArray("folders").add(folder);

    return libraryFile;
  }

//---------------------------------------------------------------------------

  private static JsonObj mendeleyDocument(String id, String title)
  {
    JsonObj document = new JsonObj(), author = new JsonObj();

    author.put("first_name", "Zoë");  // e followed by a combining diaeresis (U+0308)
    author.put("last_name", "Alfa");

    document.put("id", id);
    document.put("type", "book");
    document.put("title", title);
    document.put("abstract", "Café " + id);  // A field Hypernomicon does not manage, decomposed like the title
    document.put("year", 2001L);
    document.put("profile_id", "echo");
    document.put("last_modified", "2026-01-02T03:04:05.000Z");
    document.getOrAddArray("authors").add(author);
    document.getOrAddArray("folder_uuids").add("delta");

    JsonObj backupItem = document.deepCopy();  // The server's version as last seen; see zoteroItem

    backupItem.put("title", title + " as last synced");

    document.put("backupItem", backupItem);

    return document;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Asserts that two library files hold the same data. The arrays under the root (the entries, the trash and
   * the collections) are compared entity by entity under the given key field, since the application writes
   * them in the order of its hash maps; everything inside an entity is compared in order.
   */
  private static void assertSameLibraryFile(JsonObj expected, JsonObj actual, String keyField)
  {
    assertEquals(expected.keySet(), actual.keySet(), "root keys");

    for (String node : expected.keySet())
    {
      Map<String, JsonObj> expectedEntities = entitiesByKey(expected.getArray(node), keyField),
                           actualEntities   = entitiesByKey(actual  .getArray(node), keyField);

      assertEquals(expectedEntities.keySet(), actualEntities.keySet(), node + " keys");

      expectedEntities.forEach((key, entity) -> assertSameJson(node + '/' + key, entity, actualEntities.get(key)));
    }
  }

//---------------------------------------------------------------------------

  private static Map<String, JsonObj> entitiesByKey(JsonArray entities, String keyField)
  {
    Map<String, JsonObj> map = new TreeMap<>();

    entities.getObjs().forEach(entity -> map.put(entity.getStr(keyField), entity));

    return map;
  }

//---------------------------------------------------------------------------

  private static void assertSameJson(String path, JsonObj expected, JsonObj actual)
  {
    assertEquals(expected.keySet(), actual.keySet(), path + " keys");

    for (String key : expected.keySet())
    {
      String keyPath = path + '.' + key;
      JsonNodeType type = expected.getType(key);

      assertEquals(type, actual.getType(key), keyPath + " type");

      switch (type)
      {
        case OBJECT -> assertSameJson(keyPath, expected.getObj  (key), actual.getObj  (key));
        case ARRAY  -> assertSameJson(keyPath, expected.getArray(key), actual.getArray(key));
        default     -> assertEquals(expected.getAsStr(key), actual.getAsStr(key), keyPath);
      }
    }
  }

//---------------------------------------------------------------------------

  private static void assertSameJson(String path, JsonArray expected, JsonArray actual)
  {
    assertEquals(expected.size(), actual.size(), path + " size");

    for (int ndx = 0; ndx < expected.size(); ndx++)
    {
      String ndxPath = path + '[' + ndx + ']';
      JsonNodeType type = expected.getType(ndx);

      assertEquals(type, actual.getType(ndx), ndxPath + " type");

      switch (type)
      {
        case OBJECT -> assertSameJson(ndxPath, expected.getObj  (ndx), actual.getObj  (ndx));
        case ARRAY  -> assertSameJson(ndxPath, expected.getArray(ndx), actual.getArray(ndx));
        default     -> assertEquals(expected.getAsStr(ndx), actual.getAsStr(ndx), ndxPath);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
