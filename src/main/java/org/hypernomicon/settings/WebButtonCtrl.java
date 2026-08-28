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

package org.hypernomicon.settings;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.apache.commons.lang3.mutable.MutableInt;

import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;

//---------------------------------------------------------------------------

abstract class WebButtonCtrl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final String CUSTOM_NAME = "Custom",
                      LIBRARY_OF_CONGRESS_NAME = "Library of Congress";

  /**
   * Presets that have been replaced, mapped to what replaced them.
   * <p>
   * Preferences record a non-custom preset only by name, so a name that no longer exists in
   * the preset list is silently dropped by {@link #loadPref}. For a slot within the defaults
   * list that means quietly reverting to the default; for a slot beyond it, nothing is put in
   * the button map at all, and the slot's button ends up with no caption. Remapping the old
   * name avoids both, and preserves the user's intent of having chosen a library catalog search.
   * </p>
   */
  private static final Map<String, String> LEGACY_PRESET_NAMES = Map.of("WorldCat", LIBRARY_OF_CONGRESS_NAME);

  final String prefKey;
  final List<WebButton> webBtnList;

//---------------------------------------------------------------------------

  WebButtonCtrl(String prefKey, List<WebButton> webBtnList)
  {
    this.prefKey = prefKey;
    this.webBtnList = webBtnList;
  }

//---------------------------------------------------------------------------

  abstract void saveToPrefNode(Preferences node);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void loadPref(Preferences node, List<WebButton> srchList, String prefKey) throws BackingStoreException
  {
    loadPref(node, srchList, prefKey, null);
  }

  static void loadPref(Preferences node, List<WebButton> srchList, String prefKey, MutableInt numCustom) throws BackingStoreException
  {
    String name = node.get(prefKey, ""),
           caption = app.prefs.node("webButtonCaptions").get(prefKey, "");

    if (name.isEmpty()) return;

    String replacementName = LEGACY_PRESET_NAMES.get(name);

    if (replacementName != null)
    {
      name = replacementName;
      caption = "";  // The saved caption names the retired service, so let the new preset supply its own
    }

    for (WebButton btn : srchList)
    {
      if (btn.getName().equals(name))
      {
        if (caption.length() > 0)
          btn.setCaption(caption);

        ui.webButtonMap.put(prefKey, btn);
        return;
      }
    }

    if (name.startsWith(CUSTOM_NAME) == false) return;

    if (numCustom != null) numCustom.increment();
    WebButton webBtn = new WebButton(CUSTOM_NAME + nullSwitch(numCustom, "", MutableInt::toString), caption);
    Preferences subNode = node.node(prefKey);

    for (String patternKey : subNode.childrenNames())
    {
      Preferences patternNode = subNode.node(patternKey);
      String str = patternNode.get("str", "");
      EnumSet<WebButtonField> reqFields = EnumSet.noneOf(WebButtonField.class);

      for (int fieldCount = patternNode.getInt("reqFieldCnt", 0), fieldNdx = 1; fieldNdx <= fieldCount; fieldNdx++)
      {
        String fieldName = patternNode.get("reqField" + fieldNdx, "");
        if (fieldName.length() > 0)
          reqFields.add(WebButtonField.valueOf(fieldName));
      }

      webBtn.addPattern(str, reqFields);
    }

    ui.webButtonMap.put(prefKey, webBtn);
    srchList.add(webBtn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void saveToPrefNode(Preferences node, String prefKey, WebButton webBtn)
  {
    ui.webButtonMap.put(prefKey, webBtn);

    node.put(prefKey, webBtn.getName());

    if (webBtn.getName().startsWith(CUSTOM_NAME))
    {
      Preferences subNode = node.node(prefKey);
      try
      {
        subNode.clear();
      }
      catch (BackingStoreException e)
      {
        logThrowable(e);
      }

      int ndx = 1;
      for (UrlPattern pattern : webBtn.getPatterns())
      {
        Preferences patternNode = subNode.node("pattern" + String.format("%05d", ndx++));
        patternNode.put("str", pattern.str);

        patternNode.putInt("reqFieldCnt", pattern.reqFields().size());

        int fieldNdx = 1;
        for (WebButtonField field : pattern.reqFields())
          patternNode.put("reqField" + fieldNdx++, field.name());
      }
    }

    app.prefs.node("webButtonCaptions").put(prefKey, webBtn.getCaption());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
