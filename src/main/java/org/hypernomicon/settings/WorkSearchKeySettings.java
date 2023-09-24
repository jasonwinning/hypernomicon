/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.settings;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.records.HDT_Work;

import com.google.common.collect.ForwardingList;

import java.util.Map.Entry;

//---------------------------------------------------------------------------

public class WorkSearchKeySettings extends ForwardingList<org.hypernomicon.settings.WorkSearchKeySettings.WorkSearchKeyConfig>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum FinalConjunctionSymbol
  {
    and("and"),
    ampersand("ampersand"),
    none("");

    FinalConjunctionSymbol(String prefVal) { this.prefVal = prefVal; }

    final String prefVal;

    static FinalConjunctionSymbol fromPrefVal(String prefVal)
    {
      return Arrays.stream(values()).filter(enumVal -> prefVal.equals(enumVal.prefVal)).findFirst().orElse(none);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum CitationParenthesesOption
  {
    aroundAll("around_all"),
    aroundYear("around_year"),
    none("");

    CitationParenthesesOption(String prefVal) { this.prefVal = prefVal; }

    final String prefVal;

    static CitationParenthesesOption fromPrefVal(String prefVal)
    {
      return Arrays.stream(values()).filter(enumVal -> prefVal.equals(enumVal.prefVal)).findFirst().orElse(none);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class WorkSearchKeyConfig
  {
    final String beforeYearSep, afterNameSep, truncationIndicator;
    final boolean oxfordSeparator;
    final FinalConjunctionSymbol finalConjSymbol;
    final int authorNumToTruncate, authorsToShowWhenTruncating;

    public final CitationParenthesesOption parentheses;
    public final boolean multipleAuthors;

    private static final String beforeYearSepPrefKey = "beforeYearSep",
                                afterNameSepPrefKey = "afterNameSep",
                                truncationIndicatorPrefKey = "truncationIndicator",
                                multipleAuthorsPrefKey = "multipleAuthors",
                                oxfordSeparatorPrefKey = "oxfordSeparator",
                                finalConjSymbolPrefKey = "finalConjSymbol",
                                parenthesesPrefKey = "parentheses",
                                authorNumToTruncatePrefKey = "authorNumToTruncate",
                                authorsToShowWhenTruncatingPrefKey = "authorsToShowWhenTruncating";

//---------------------------------------------------------------------------

    WorkSearchKeyConfig(String beforeYearSep, String afterNameSep, String truncationIndicator,
                        boolean multipleAuthors, boolean oxfordSeparator,
                        FinalConjunctionSymbol finalConjSymbol, CitationParenthesesOption parentheses,
                        int authorNumToTruncate, int authorsToShowWhenTruncating)
    {
      this.beforeYearSep = beforeYearSep;
      this.afterNameSep = afterNameSep;
      this.truncationIndicator = truncationIndicator;
      this.multipleAuthors = multipleAuthors;
      this.oxfordSeparator = oxfordSeparator;
      this.finalConjSymbol = finalConjSymbol;
      this.parentheses = parentheses;
      this.authorNumToTruncate = authorNumToTruncate;
      this.authorsToShowWhenTruncating = authorsToShowWhenTruncating;
    }

//---------------------------------------------------------------------------

    WorkSearchKeyConfig() { this(db.prefs.node("workSearchKeys")); }   // This node won't have any values so the config object will be populated with defaults

//---------------------------------------------------------------------------

    private WorkSearchKeyConfig(Preferences node)
    {
      beforeYearSep       = node.get(beforeYearSepPrefKey, " ");
      afterNameSep        = node.get(afterNameSepPrefKey, ", ");
      truncationIndicator = node.get(truncationIndicatorPrefKey, " et al.");

      multipleAuthors     = node.getBoolean(multipleAuthorsPrefKey, false);
      oxfordSeparator     = node.getBoolean(oxfordSeparatorPrefKey, true );

      authorNumToTruncate         = node.getInt(authorNumToTruncatePrefKey        , 4);
      authorsToShowWhenTruncating = node.getInt(authorsToShowWhenTruncatingPrefKey, 2);

      finalConjSymbol = FinalConjunctionSymbol   .fromPrefVal(node.get(finalConjSymbolPrefKey, FinalConjunctionSymbol   .ampersand.prefVal));
      parentheses     = CitationParenthesesOption.fromPrefVal(node.get(parenthesesPrefKey    , CitationParenthesesOption.none     .prefVal));
    }

//---------------------------------------------------------------------------

    private void saveToPrefNode(Preferences node)
    {
      node.put(beforeYearSepPrefKey         , beforeYearSep);
      node.put(afterNameSepPrefKey          , afterNameSep);
      node.put(truncationIndicatorPrefKey   , truncationIndicator);

      node.putBoolean(multipleAuthorsPrefKey, multipleAuthors);
      node.putBoolean(oxfordSeparatorPrefKey, oxfordSeparator);

      node.putInt(authorNumToTruncatePrefKey        , authorNumToTruncate);
      node.putInt(authorsToShowWhenTruncatingPrefKey, authorsToShowWhenTruncating);

      node.put(finalConjSymbolPrefKey, finalConjSymbol.prefVal);
      node.put(parenthesesPrefKey    , parentheses.prefVal);
    }

//---------------------------------------------------------------------------

    public String format(List<String> authors, String year)
    {
      StringBuilder str = new StringBuilder(parentheses == CitationParenthesesOption.aroundAll ? "(" : "");

      int authorsToShow = appendCitationAuthors(str, authors, truncationIndicator, multipleAuthors, oxfordSeparator, authorNumToTruncate, authorsToShowWhenTruncating, finalConjSymbol);

      if (year.isBlank() == false)
      {
        if (authorsToShow > 0)
          str.append(beforeYearSep);

        if (parentheses == CitationParenthesesOption.aroundYear)
          str.append('(');

        str.append(ultraTrim(year));

        if (parentheses == CitationParenthesesOption.aroundYear)
          str.append(')');
      }

      return (parentheses == CitationParenthesesOption.aroundAll ? (str + ")") : str.toString()).replaceAll("  ", " ");
    }

//---------------------------------------------------------------------------

    static int appendCitationAuthors(StringBuilder str, List<String> authors, String truncationIndicator, boolean multipleAuthors, boolean oxfordSeparator, int authorNumToTruncate, int authorsToShowWhenTruncating, FinalConjunctionSymbol finalConjSymbol)
    {
      int authorsToShow = authors.size();

      if ((authorsToShow > 1) && (multipleAuthors == false))
        authorsToShow = 1;
      else if ((authorNumToTruncate > 0) && (authorNumToTruncate <= authorsToShow))
        authorsToShow = Math.min(authorsToShowWhenTruncating, (authorNumToTruncate - 1));

      for (int ndx = 0; ndx < authorsToShow; ndx++)
      {
        if ((ndx > 0) && (authorsToShow != 2) && ((finalConjSymbol == FinalConjunctionSymbol.none) || oxfordSeparator || (ndx < (authors.size() - 1))))
          str.append(", ");

        if ((ndx > 1) && (ndx == (authors.size() - 1)) && (finalConjSymbol != FinalConjunctionSymbol.none))
          str.append(finalConjSymbol == FinalConjunctionSymbol.and ? " and " : " & ");
        else if ((ndx == 1) && (authorsToShow == 2))
        {
          switch (finalConjSymbol)
          {
            case and       : str.append(" and "); break;
            case ampersand : str.append(" & ");   break;
            default        : str.append(", ");    break;
          }
        }

        str.append(authors.get(ndx));
      }

      if (multipleAuthors && (authorsToShow < authors.size()))
        str.append(truncationIndicator);

      return authorsToShow;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<WorkSearchKeyConfig> keyConfigList = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected List<WorkSearchKeyConfig> delegate()
  {
    return keyConfigList;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static WorkSearchKeySettings loadFromPrefNode()
  {
    WorkSearchKeySettings settings = new WorkSearchKeySettings();

    Preferences node = db.prefs.node("workSearchKeys");

    int count = node.getInt("count", 0);

    if (count == 0)
    {
      settings.add(new WorkSearchKeyConfig());
      return settings;
    }

    for (int ndx = 0; ndx < count; ndx++)
      settings.add(new WorkSearchKeyConfig(node.node("key" + ndx)));

    return settings;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToPrefNode()
  {
    Preferences node = db.prefs.node("workSearchKeys");
    try { node.removeNode(); } catch (BackingStoreException e) { throw new AssertionError(e); }
    node = db.prefs.node("workSearchKeys");

    node.putInt("count", size());

    for (int ndx = 0; ndx < size(); ndx++)
      get(ndx).saveToPrefNode(node.node("key" + ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String format(List<String> authors, String year)
  {
    return format(authors, authors.get(0), year, false, null, false);
  }

  public String format(List<String> authors, String singleAuthorName, String year, boolean addLetter, HDT_Work work, boolean keyWorkLink)
  {
    Map<String, Boolean> keyToMultipleAuthors = new LinkedHashMap<>();

    String singleAuthorKey = "";

    for (WorkSearchKeyConfig keyConfig : this)
    {
      List<String> keyAuthors = keyConfig.multipleAuthors ? authors : List.of(singleAuthorName);
      String key = keyConfig.format(keyAuthors, year);
      if (singleAuthorKey.isBlank() && (keyConfig.multipleAuthors == false))
        singleAuthorKey = key;

      keyToMultipleAuthors.put(key, keyAuthors.size() > 1);
    }

    if (addLetter)
    {
      keyToMultipleAuthors = addLetterToKeys(keyToMultipleAuthors, year, work, true );
      keyToMultipleAuthors = addLetterToKeys(keyToMultipleAuthors, year, work, false);
    }

    if (keyWorkLink == false)
      return SearchKeys.prepSearchKey(keyToMultipleAuthors.keySet().stream().reduce((s1, s2) -> s1 + "; " + s2).orElse(""));

    // Use first key configured as single author

    if (singleAuthorKey.isBlank() == false)
      return singleAuthorKey;

    // No keys are configured as single author so just return the first key

    return SearchKeys.prepSearchKey(keyToMultipleAuthors.keySet().iterator().next());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * <p>We want to make sure the same letter is added to all single-author key, and the same letter is added to all multiple-author keys.</p>
   *
   * <p>This function figures out if adding a certain letter will result in a duplicate key for any of the keys that have the same mulitipleAuthors value
   * for their corresponding WorkSearchKeyConfig. If so, it tries a different letter. Otherwise, it returns a new map with the key strings updated
   * to have the new letter appended to the year.</p>
   *
   * @param oldMap  Mapping of key strings with their corresponding WorkSearchKeyConfig objects
   * @param year  Updated year for the work in question
   * @param work  The work that these search keys are for
   * @param multipleAuthors  Whether we are adding a letter to single-author or mult-author search keys this time around
   * @return  The original map if no letter is being added to the years, or a new version with letter added
   */
  private static Map<String, Boolean> addLetterToKeys(Map<String, Boolean> oldMap, String year, HDT_Work work, boolean multipleAuthors)
  {
    char keyLetter = ' ';
    boolean keyTaken;

    do
    {
      keyTaken = false;

      for (Entry<String, Boolean> entry : oldMap.entrySet())
      {
        if (entry.getValue() != multipleAuthors)
          continue;

        SearchKeyword hyperKey = db.getKeyByKeyword(entry.getKey().replace(year, (year + keyLetter).trim()));

        if ((hyperKey != null) && (hyperKey.record != work))
        {
          keyTaken = true;

          if (keyLetter == 'z')
            return oldMap;  // Unlikely scenario. If every letter of the alphabet was already used, let user come up with non-duplicate keys manually.

          keyLetter = keyLetter == ' ' ? 'a' : (char)(keyLetter + 1);

          break;
        }
      }

    } while (keyTaken);

    if (keyLetter == ' ')
      return oldMap;

    Map<String, Boolean> newMap = new LinkedHashMap<>();

    for (Entry<String, Boolean> entry : oldMap.entrySet())
    {
      if (entry.getValue() != multipleAuthors)
        newMap.put(entry.getKey(), entry.getValue());
      else
        newMap.put(entry.getKey().replace(year, (year + keyLetter).trim()), entry.getValue());
    }

    return newMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
