/*
 * Copyright 2015-2020 Jason Winning
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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Predicate;

import org.apache.http.client.methods.HttpUriRequest;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.ImmutableSet;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.workMerge.MergeWorksDlgCtrlr;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public abstract class LibraryWrapper<BibEntry_T extends BibEntry, BibCollection_T extends BibCollection>
{
  //---------------------------------------------------------------------------

  public static enum LibraryType
  {
    ltZotero("zotero", "Zotero"),
    ltMendeley("mendeley", "Mendeley");

    private LibraryType(String descriptor, String userFriendlyName) { this.descriptor = descriptor; this.userFriendlyName = userFriendlyName; }
    private final String descriptor, userFriendlyName;

    public String getDescriptor()       { return descriptor; }
    public String getUserFriendlyName() { return userFriendlyName; }

    public static LibraryType getByDescriptor(String descriptor)
    {
      return findFirst(EnumSet.allOf(LibraryType.class), type -> type.descriptor.equals(descriptor));
    }
  }

  //---------------------------------------------------------------------------

  public static abstract class SyncTask extends HyperTask
  {
    public SyncTask() { super("SyncReferenceLibrary"); }

    protected boolean changed = false;
    public boolean getChanged() { return changed; }
  }

  private final List<String> keyList = new ArrayList<>();

  protected final Map<String, BibEntry_T> keyToAllEntry = new HashMap<>(), keyToTrashEntry = new HashMap<>();
  protected final Map<String, BibCollection_T> keyToColl = new HashMap<>();

  protected final JsonHttpClient jsonClient = new JsonHttpClient();
  protected SyncTask syncTask = null;
  protected HttpUriRequest request = null;
  private BiConsumer<String, String> keyChangeHndlr;

  protected boolean didMergeDuringSync = false;

  public abstract SyncTask createNewSyncTask();
  public abstract void loadFromDisk(FilePath filePath) throws FileNotFoundException, IOException, ParseException;
  public abstract LibraryType type();
  public abstract EnumHashBiMap<EntryType, String> getEntryTypeMap();
  public abstract String getHtml(BibEntryRow row);
  public abstract void safePrefs();
  public abstract String entryFileNode();
  public abstract String collectionFileNode();

  public final Set<BibCollection_T> getColls()           { return new LinkedHashSet<>(keyToColl.values()); }
  public final Set<BibEntry_T> getTrash()                { return new LinkedHashSet<>(keyToTrashEntry.values()); }
  public final Set<BibEntry_T> getAllEntries()           { return new LinkedHashSet<>(keyToAllEntry.values()); }
  public final Map<String, BibCollection> getKeyToColl() { return Collections.unmodifiableMap(keyToColl); }

  public BibEntry_T getEntryByKey(String key)            { return keyToAllEntry.get(key); }
  public BibEntry_T getEntryByID(int id)                 { return keyToAllEntry.get(keyList.get(id - 1)); }

  public final void setKeyChangeHandler(BiConsumer<String, String> hndlr) { keyChangeHndlr = hndlr; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  protected final void clear()
  {
    keyToAllEntry.clear();
    keyToTrashEntry.clear();
    keyToColl.clear();
    keyList.clear();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public final void stop()
  {
    if ((syncTask != null) && syncTask.isRunning())
      syncTask.cancel();

    if (request != null)
      request.abort();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  protected void updateKey(String oldKey, String newKey)
  {
    if (keyToAllEntry.containsKey(oldKey))
      keyToAllEntry.put(newKey, keyToAllEntry.remove(oldKey));

    if (keyToTrashEntry.containsKey(oldKey))
      keyToTrashEntry.put(newKey, keyToTrashEntry.remove(oldKey));

    if (keyToColl.containsKey(oldKey))
      keyToColl.put(newKey, keyToColl.remove(oldKey));

    nullSwitch(db.getWorkByBibEntryKey(oldKey), work -> work.setBibEntryKey(newKey));

    keyChangeHndlr.accept(oldKey, newKey);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  protected static String anchorTag(String text, String url)
  {
    return "<a href=\"\" onclick=\"openURL('" + url + "'); return false;\">" + text + "</a>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected String compileHtml()
  {
    StringBuilder html = new StringBuilder()

        .append("<html><head>" + MainTextUtil.scriptContent + "<style>")
        .append("td.fieldName { vertical-align: text-top; text-align: right; padding-right:10px; }</style></head><body>")
        .append("<table style=\"font-size:9pt; font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen-Sans,Ubuntu,Cantarell,sans-serif; line-height:10pt;\">");

    List<String> fieldOrder = getHtmlFieldOrder();

    for (String fieldName : fieldOrder)
    {
      Iterator<FieldHtml> it = fieldHtmlList.iterator();

      while (it.hasNext())
      {
        FieldHtml fieldHtml = it.next();

        if (fieldHtml.fieldName.equalsIgnoreCase(fieldName))
        {
          html.append(fieldHtml.html);
          it.remove();
        }
      }
    }

    fieldHtmlList.forEach(fieldHtml -> html.append(fieldHtml.html));

    fieldHtmlList.clear();

    return html.append("</table></body></html>").toString();
  }

  protected abstract List<String> getHtmlFieldOrder();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static String makeHtmlRow(String fieldName, String value)
  {
    return "<tr><td class=\"fieldName\">" + fieldName + "</td><td>" +
           value +
           "</td></tr>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class FieldHtml
  {
    public final String fieldName;
    public final String html;

    private FieldHtml(String fieldName, String html)
    {
      this.fieldName = fieldName;
      this.html = html;
    }
  }

  private final List<FieldHtml> fieldHtmlList = new ArrayList<>();

  protected void addFieldHtml(String fieldName, String html)
  {
    if (safeStr(html).isBlank()) return;

    fieldHtmlList.add(new FieldHtml(fieldName, html));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static String makeHtmlRows(String fieldName, List<String> list)
  {
    list.removeIf(str -> str == null);

    if (list.size() == 0) return "";

    StringBuilder html = new StringBuilder("<tr><td class=\"fieldName\">" + fieldName + "</td><td>");

    for (int ndx = 0; ndx < list.size(); ndx++)
    {
      html.append(list.get(ndx));
      if (ndx < (list.size() - 1))
        html.append("<br>");
    }

    return html.append("</td></tr>").toString();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private Boolean fxThreadReturnValue = null;

  public boolean doMerge(BibEntry_T entry, JsonObj jObj)
  {
    fxThreadReturnValue = null;

    runInFXThread(() ->
    {
      MergeWorksDlgCtrlr mwd = null;

      try
      {
        mwd = MergeWorksDlgCtrlr.create("Merge Remote Changes with Local Changes", entry, BibEntry.create(type(), this, jObj, true),
                                        null, null, entry.getWork(), false, false, false);
      }
      catch (IOException e)
      {
        messageDialog("Unable to initialize merge dialog window.", mtError);
        fxThreadReturnValue = Boolean.FALSE;
        return;
      }

      entry.update(jObj, true, true);

      if (mwd.showModal())
        mwd.mergeInto(entry);

      fxThreadReturnValue = Boolean.TRUE;
    });

    while (fxThreadReturnValue == null) sleepForMillis(100);

    didMergeDuringSync = true;

    return fxThreadReturnValue.booleanValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntry_T addEntry(EntryType newType)
  {
    BibEntry_T item = BibEntry.create(type(), this, newType);

    keyToAllEntry.put(item.getKey(), item);

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<BibEntry_T> getCollectionEntries(String collKey)
  {
    return getNonTrashEntries().stream().filter(item -> item.getCollKeys(false).contains(collKey))
                                        .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<BibEntry_T> getNonTrashEntries()
  {
    return keyToAllEntry.values().stream().filter(item -> keyToTrashEntry.containsKey(item.getKey()) == false)
                                          .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<BibEntry_T> getUnsorted()
  {
    Predicate<BibEntry_T> predicate = item ->
    {
      if (keyToTrashEntry.containsKey(item.getKey()))
        return false;

      return item.getCollKeys(true).stream().noneMatch(keyToColl::containsKey);
    };

    return keyToAllEntry.values().stream().filter(predicate).collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void loadFromJSON(JsonObj jObj)
  {
    jObj.getArray(entryFileNode()).getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(type(), this, itemJsonObj, false);

      if (entry != null)
        keyToAllEntry.put(entry.getKey(), entry);
    });

    nullSwitch(jObj.getArray("trash"), jArr -> jArr.getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(type(), this, itemJsonObj, false);

      if (entry == null) return;

      keyToAllEntry.put(entry.getKey(), entry);
      keyToTrashEntry.put(entry.getKey(), entry);
    }));

    nullSwitch(jObj.getArray(collectionFileNode()), jArr -> jArr.getObjs().forEach(collJsonObj ->
    {
      BibCollection_T coll = BibCollection.create(type(), collJsonObj);

      if (coll != null)
        keyToColl.put(coll.getKey(), coll);
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToDisk()
  {
    StringBuilder json = null;

    try
    {
      JsonObj jMainObj = new JsonObj();

      JsonArray jArr = new JsonArray();

      for (BibEntry_T entry : getNonTrashEntries())
        entry.saveToDisk(jArr);

      jMainObj.put(entryFileNode(), jArr);

      jArr = new JsonArray();

      for (BibEntry_T entry : keyToTrashEntry.values())
        entry.saveToDisk(jArr);

      jMainObj.put("trash", jArr);

      jArr = new JsonArray();

      for (BibCollection_T coll : keyToColl.values())
        coll.saveToDisk(jArr);

      jMainObj.put(collectionFileNode(), jArr);

      json = new StringBuilder(jMainObj.toString());
    }
    catch (Throwable e)
    {
      showStackTrace(e);
      return;
    }

    try
    {
      saveStringBuilderToFile(json, db.xmlPath(BIB_FILE_NAME));
      safePrefs();
    }
    catch (IOException e)
    {
      messageDialog("An error occurred while saving bibliographic data to disk.", mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int numericID(String key)
  {
    int ndx = keyList.indexOf(key);

    if (ndx < 0)
    {
      ndx = keyList.size();
      keyList.add(key);
    }

    return ndx + 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
