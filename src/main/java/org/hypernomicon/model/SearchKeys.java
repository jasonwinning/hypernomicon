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

package org.hypernomicon.model;

import static org.hypernomicon.model.HyperDB.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.hypernomicon.model.Exceptions.DuplicateSearchKeyException;
import org.hypernomicon.model.Exceptions.SearchKeyTooShortException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.util.SplitString;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public final class SearchKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class SearchKeyword
  {
    public final String text;
    public final boolean startOnly, endOnly;
    public final HDT_Record record;

  //---------------------------------------------------------------------------

    public SearchKeyword(String newKeyword, HDT_Record record)
    {
      this.record = record;

      newKeyword = newKeyword.strip();

      if (newKeyword.isEmpty())
      {
        text = "";
        startOnly = false;
        endOnly = false;
        return;
      }

      if (newKeyword.startsWith("^"))
      {
        startOnly = true;
        newKeyword = newKeyword.substring(1);
      }
      else
        startOnly = false;

      if (newKeyword.endsWith("$"))
      {
        endOnly = true;
        newKeyword = newKeyword.substring(0, newKeyword.length() - 1);
      }
      else
        endOnly = false;

      text = newKeyword;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public String toString()  { return (startOnly ? '^' + text : text) + (endOnly ? '$' : ""); }
    private String getPrefix()          { return text.substring(0, 3).toLowerCase(); }
    private HDT_Record getRecord()      { return record; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  }

  private final Map<String    , Map<String, SearchKeyword>> prefixStrToKeywordStrToKeywordObj;
  private final Map<HDT_Record, Map<String, SearchKeyword>> recordToKeywordStrToKeywordObj;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SearchKeys()
  {
    prefixStrToKeywordStrToKeywordObj = new ConcurrentHashMap<>();
    recordToKeywordStrToKeywordObj    = new ConcurrentHashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeAll()
  {
    prefixStrToKeywordStrToKeywordObj.clear();
    recordToKeywordStrToKeywordObj   .clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Iterable<SearchKeyword> getKeywordsByPrefix(String prefix)
  {
    return nullSwitch(prefixStrToKeywordStrToKeywordObj.get(prefix.toLowerCase()), Collections.emptyList(), map -> Collections.unmodifiableCollection(map.values()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Iterable<SearchKeyword> getKeysByRecord(HDT_Record record)
  {
    return nullSwitch(recordToKeywordStrToKeywordObj.get(record), Collections.emptyList(), map -> Collections.unmodifiableCollection(map.values()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String prepSearchKey(String newKey)
  {
    newKey = newKey.strip().replaceAll("\\h+", " ");

    return convertToEnglishChars(newKey).replaceAll("\\p{Pd}", "-"); // treat all dashes the same within search keyword
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setSearchKey(HDT_Record record, String newKey, boolean noMod, boolean rebuildMentions) throws DuplicateSearchKeyException, SearchKeyTooShortException
  {
    newKey = prepSearchKey(newKey);

    if (newKey.equals(getStringForRecord(record))) return;

    if ((newKey.length() == 1) || (newKey.length() == 2))
      throw new SearchKeyTooShortException(record, newKey);

    LinkedHashSet<SearchKeyword> oldKeywordObjs = unassignKeywordsFromRecord(record);

  // Loop through new substrings
  // ---------------------------
    for (String subStr : new SplitString(newKey, ';'))
    {
      SearchKeyword keyword = new SearchKeyword(subStr.strip(), record);

      if (keyword.text.isEmpty()) continue;

  // If the substring is too short, error out
  // ----------------------------------------
      if (keyword.text.length() < 3)
      {
        assignKeywordsToRecord(record, oldKeywordObjs);
        throw new SearchKeyTooShortException(record, keyword.text);
      }

      HDT_Record existingRecord = nullSwitch(getKeywordObjByKeywordStr(keyword.text), null, SearchKeyword::getRecord);

  // If the substring was already a key for a different record, error out
  // --------------------------------------------------------------------
      if ((existingRecord != null) && (existingRecord != record))
      {
        assignKeywordsToRecord(record, oldKeywordObjs);
        throw new DuplicateSearchKeyException(record, keyword.text);
      }

  // Add new substring
  // -----------------
      addKeyword(keyword);
    }

    if (noMod == false)
      record.modifyNow();

    if (rebuildMentions)
      db.rebuildMentions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  SearchKeyword getKeywordObjByKeywordStr(CharSequence str)
  {
    String keywordStr = convertToEnglishChars(str).toLowerCase();

    if (keywordStr.length() < 3) return null;

    return nullSwitch(prefixStrToKeywordStrToKeywordObj.get(keywordStr.substring(0, 3)), null, map -> map.get(keywordStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String firstActiveKeyword(HDT_Record record)
  {
    Map<String, SearchKeyword> keywordStrToKeywordObj = recordToKeywordStrToKeywordObj.get(record);
    if (keywordStrToKeywordObj == null) return "";

    synchronized (keywordStrToKeywordObj)
    {
      Collection<SearchKeyword> values = keywordStrToKeywordObj.values();
      return values.isEmpty() ? "" : values.iterator().next().text;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getStringForRecord(HDT_Record record)
  {
    Map<String, SearchKeyword> keywordStrToKeywordObj = recordToKeywordStrToKeywordObj.get(record);
    if (keywordStrToKeywordObj == null) return "";

    synchronized (keywordStrToKeywordObj)
    {
      return keywordStrToKeywordObj.values().stream().map(SearchKeyword::toString).collect(Collectors.joining("; "));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addKeyword(SearchKeyword keyword)
  {
    String lcText = keyword.text.toLowerCase();

    recordToKeywordStrToKeywordObj   .computeIfAbsent(keyword.record     , _ -> Collections.synchronizedMap(new LinkedHashMap<>())).put(lcText, keyword);
    prefixStrToKeywordStrToKeywordObj.computeIfAbsent(keyword.getPrefix(), _ -> Collections.synchronizedMap(new LinkedHashMap<>())).put(lcText, keyword);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignKeywordsToRecord(HDT_Record record, Iterable<SearchKeyword> oldKeywordObjs)
  {
    unassignKeywordsFromRecord(record);
    oldKeywordObjs.forEach(this::addKeyword);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private LinkedHashSet<SearchKeyword> unassignKeywordsFromRecord(HDT_Record record)
  {
    LinkedHashSet<SearchKeyword> oldKeywordObjs = new LinkedHashSet<>();

    nullSwitch(recordToKeywordStrToKeywordObj.get(record), map -> { synchronized (map) { map.entrySet().removeIf(entry ->
    {
      SearchKeyword keyword = entry.getValue();
      oldKeywordObjs.add(keyword);

      String prefix = keyword.getPrefix();

      Map<String, SearchKeyword> map2 = prefixStrToKeywordStrToKeywordObj.get(prefix);
      map2.remove(keyword.text.toLowerCase());

      if (map2.isEmpty())
        prefixStrToKeywordStrToKeywordObj.remove(prefix);

      return true;
    }); }});

    return oldKeywordObjs;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
