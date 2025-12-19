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

package org.hypernomicon.model.searchKeys;

import static org.hypernomicon.model.HyperDB.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.hypernomicon.model.Exceptions.DuplicateSearchKeyException;
import org.hypernomicon.model.Exceptions.SearchKeyTooShortException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.util.SplitString;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public final class SearchKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Map<String    , Map<String, Keyword>>        prefixStrToKeywordStrToKeywordObj;
  private final Map<HDT_Record, Map<String, KeywordBinding>> recordToKeywordStrToBinding;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SearchKeys()
  {
    prefixStrToKeywordStrToKeywordObj = new ConcurrentHashMap<>();
    recordToKeywordStrToBinding       = new ConcurrentHashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeAll()
  {
    prefixStrToKeywordStrToKeywordObj.clear();
    recordToKeywordStrToBinding      .clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Keyword getKeywordObjByKeywordStr(String str)
  {
    String keywordStr = KeywordBinding.normalizeText(str, true);

    if (keywordStr.length() < 3) return null;

    return nullSwitch(prefixStrToKeywordStrToKeywordObj.get(keywordStr.substring(0, 3)), null, map -> map.get(keywordStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Iterable<Keyword> getKeywordsByPrefix(String prefix)
  {
    return nullSwitch(prefixStrToKeywordStrToKeywordObj.get(prefix.toLowerCase()), Collections.emptyList(), map -> Collections.unmodifiableCollection(map.values()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Iterable<KeywordBinding> getKeysByRecord(HDT_Record record)
  {
    return nullSwitch(recordToKeywordStrToBinding.get(record), Collections.emptyList(), map -> Collections.unmodifiableCollection(map.values()));
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

  public void setSearchKey(HDT_Record record, String newKey, boolean noMod, boolean rebuildMentions, boolean confirmDup) throws DuplicateSearchKeyException, SearchKeyTooShortException
  {
    newKey = prepSearchKey(newKey);

    if ((newKey.length() == 1) || (newKey.length() == 2))
      throw new SearchKeyTooShortException(record, newKey);

    String otherStr = getStringForRecord(record);

    if (sameSearchKeys(newKey, otherStr))
      return;

    LinkedHashSet<KeywordBinding> oldBindings = unassignBindingsFromRecord(record);

  // Loop through new substrings
  // ---------------------------
    for (String subStr : new SplitString(newKey, ';'))
    {
      KeywordBinding binding = new KeywordBinding(subStr, record);

      if (binding.getUserText().isBlank()) continue;

  // If the substring is too short, error out
  // ----------------------------------------
      if (binding.getUserText().length() < 3)
      {
        assignBindingsToRecord(record, oldBindings);
        throw new SearchKeyTooShortException(record, binding.getUserText());
      }

      if (confirmDup)
      {
        Keyword existingKeyObj = getKeywordObjByKeywordStr(binding.getNormalizedText());

  // If the substring was already a key for a different record, error out
  // --------------------------------------------------------------------
        if ((existingKeyObj != null) && (existingKeyObj.getAllRecords().contains(record) == false) &&
            oldBindings.stream().noneMatch(oldBinding -> oldBinding.getNormalizedText().equals(binding.getNormalizedText())))
        {
          HDT_Record existingRecord = existingKeyObj.getAllRecords().iterator().next();

          if (confirmDialog("The search keyword \"" + binding.getUserText() + "\" is already in use by record:\n\n" +
                            "Type: " + getTypeName(existingRecord.getType()) + '\n' +
                            "Name: " + existingRecord.name() + '\n' +
                            "ID: " + existingRecord.getID() + "\n\n" +
                            "Continue assigning keyword to the current record?", false) == false)
          {
            assignBindingsToRecord(record, oldBindings);
            throw new DuplicateSearchKeyException(record, binding.getUserText());
          }
        }
      }

  // Add new substring
  // -----------------
      addKeywordBinding(binding);
    }

    if (noMod == false)
      record.modifyNow();

    if (rebuildMentions)
      db.rebuildMentions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String firstActiveKeyword(HDT_Record record)
  {
    Map<String, KeywordBinding> keywordStrToBinding = recordToKeywordStrToBinding.get(record);
    if (keywordStrToBinding == null) return "";

    synchronized (keywordStrToBinding)
    {
      Collection<KeywordBinding> values = keywordStrToBinding.values();
      return values.isEmpty() ? "" : values.iterator().next().getUserText();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStringForRecord(HDT_Record record)
  {
    Map<String, KeywordBinding> keywordStrToBinding = recordToKeywordStrToBinding.get(record);
    if (keywordStrToBinding == null) return "";

    synchronized (keywordStrToBinding)
    {
      return keywordStrToBinding.values().stream().map(KeywordBinding::toString).collect(Collectors.joining("; "));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addKeywordBinding(KeywordBinding binding)
  {
    String normalizedText = binding.getNormalizedText();

    recordToKeywordStrToBinding
      .computeIfAbsent(binding.getRecord(), _ -> Collections.synchronizedMap(new LinkedHashMap<>()))
      .put(normalizedText, binding);

    Map<String, Keyword> map = prefixStrToKeywordStrToKeywordObj
      .computeIfAbsent(binding.getPrefix(), _ -> Collections.synchronizedMap(new LinkedHashMap<>()));

    synchronized (map)
    {
      map.merge(normalizedText, new Keyword(binding), (existing, newKeyword) -> { existing.addBinding(binding); return existing; });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignBindingsToRecord(HDT_Record record, Iterable<KeywordBinding> oldBindings)
  {
    unassignBindingsFromRecord(record);
    oldBindings.forEach(this::addKeywordBinding);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private LinkedHashSet<KeywordBinding> unassignBindingsFromRecord(HDT_Record record)
  {
    LinkedHashSet<KeywordBinding> oldBindings = new LinkedHashSet<>();

    Map<String, KeywordBinding> map = recordToKeywordStrToBinding.get(record);
    if (map == null) return oldBindings;

    synchronized (map)
    {
      map.entrySet().removeIf(entry ->
      {
        KeywordBinding binding = entry.getValue();
        oldBindings.add(binding);

        String prefix         = binding.getPrefix(),
               normalizedText = binding.getNormalizedText();

        Map<String, Keyword> map2 = prefixStrToKeywordStrToKeywordObj.get(prefix);
        Keyword keyword = map2.get(normalizedText);

        if (keyword.removeBinding(record))
        {
          map2.remove(normalizedText);

          if (map2.isEmpty())
            prefixStrToKeywordStrToKeywordObj.remove(prefix);
        }

        return true;
      });
    }

    return oldBindings;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean sameSearchKeys(String s1, String s2)
  {
    if (strNullOrBlank(s1) && strNullOrBlank(s2))
      return true;

    if (strNullOrBlank(s1) || strNullOrBlank(s2))
      return false;

    if (s1.equals(s2))
      return true;

    Set<String> set1 = Arrays.stream(s1.split(";"))
      .map(token -> token.strip().toLowerCase())
      .collect(Collectors.toSet());

    Set<String> set2 = Arrays.stream(s2.split(";"))
      .map(token -> token.strip().toLowerCase())
      .collect(Collectors.toSet());

    return set1.equals(set2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
