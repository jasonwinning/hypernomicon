/*
 * Copyright 2015-2024 Jason Winning
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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.util.WebButton.WebButtonField.*;

import java.util.ArrayList;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.stage.Window;

public class WebButtonSettingsCtrlr implements SettingsControl
{
  @FXML private Button btnPersonImgSrchAdvanced, btnInstSrchAdvanced, btnInstMapSrchAdvanced,
                       btnDOISrchAdvanced, btnISBNSrchAdvanced;
  @FXML private ComboBox<WebButton> cbPersonImgSrch, cbInstSrch, cbInstMapSrch, cbDOISrch, cbISBNSrch;
  @FXML private TextField tfPersonImgSrch, tfDOISrch, tfISBNSrch, tfInstMapSrch;
  @FXML private TableView<HyperTableRow> tvPersonSrch, tvWorkSrch, tvGenSrch;

  private final List<WebButtonCtrl> webBtnCtrlList = new ArrayList<>();

  private static final List<WebButton> personSrchList = new ArrayList<>(), personImgSrchList = new ArrayList<>(),
                                       instSrchList   = new ArrayList<>(), instMapSrchList   = new ArrayList<>(),
                                       doiSrchList    = new ArrayList<>(), isbnSrchList      = new ArrayList<>(),
                                       workSrchList   = new ArrayList<>(), genSrchList       = new ArrayList<>(),

                                       personSrchDefaults = new ArrayList<>(),
                                       workSrchDefaults   = new ArrayList<>(),
                                       genSrchDefaults    = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(Window owner, boolean noDB)
  {
    webBtnCtrlList.addAll(List.of(

      new WebButtonTable(PREF_KEY_PERSON_SRCH, personSrchList, personSrchDefaults, tvPersonSrch),
      new WebButtonTable(PREF_KEY_WORK_SRCH  , workSrchList,   workSrchDefaults,   tvWorkSrch),
      new WebButtonTable(PREF_KEY_GEN_SRCH   , genSrchList,    genSrchDefaults,    tvGenSrch),

      new WebButtonBar(PREF_KEY_PERSON_IMG_SRCH, personImgSrchList, tfPersonImgSrch, cbPersonImgSrch, btnPersonImgSrchAdvanced),
      new WebButtonBar(PREF_KEY_INST_SRCH      , instSrchList,      null           , cbInstSrch     , btnInstSrchAdvanced),
      new WebButtonBar(PREF_KEY_INST_MAP_SRCH  , instMapSrchList,   tfInstMapSrch  , cbInstMapSrch  , btnInstMapSrchAdvanced),
      new WebButtonBar(PREF_KEY_DOI_SRCH       , doiSrchList,       tfDOISrch      , cbDOISrch      , btnDOISrchAdvanced),
      new WebButtonBar(PREF_KEY_ISBN_SRCH      , isbnSrchList,      tfISBNSrch     , cbISBNSrch     , btnISBNSrchAdvanced)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void populatePresets()
  {

 // Person search buttons --------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    WebButton btn = new WebButton("Google", "Google");

    btn.addPattern("http://www.google.com/search?q=" + FirstName + "%20" + LastName + "%20" + Field,
        FirstName, LastName);

    btn.addPattern("http://www.google.com/search?q=" + SingleName + "%20" + Field,
        SingleName);

    personSrchList.add(btn);
    personSrchDefaults.add(btn);

    btn = new WebButton("Scholar", "Scholar");

    btn.addPattern("https://scholar.google.com/scholar?q=author:%22" + QueryName + "%20" + LastName + "%22",
        QueryName, LastName);

    btn.addPattern("https://scholar.google.com/scholar?q=author:%22" + SingleName + "%22",
        SingleName);

    personSrchList.add(btn);
    personSrchDefaults.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern("https://philpapers.org/s/@author%20" + QueryName + "%20" + LastName,
        QueryName, LastName);

    btn.addPattern("https://philpapers.org/s/@author%20" + SingleName,
        SingleName);

    personSrchList.add(btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern("https://www.bing.com/search?q=" + FirstName + "%20" + LastName + "%20" + Field,
        FirstName, LastName);

    btn.addPattern("https://www.bing.com/search?q=" + SingleName + "%20" + Field,
        SingleName);

    personSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern("https://duckduckgo.com/?q=" + FirstName + "%20" + LastName + "%20" + Field,
        FirstName, LastName);

    btn.addPattern("https://duckduckgo.com/?q=" + SingleName + "%20" + Field,
        SingleName);

    personSrchList.add(btn);

 // Person image search button ---------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google Image Search", "Google");

    btn.addPattern("http://www.google.com/search?q=" + FirstName + "%20" + LastName + "%20" + Field + "&tbm=isch",
        FirstName, LastName);

    btn.addPattern("http://www.google.com/search?q=" + SingleName + "%20" + Field + "&tbm=isch",
        SingleName);

    personImgSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_PERSON_IMG_SRCH, btn);

    btn = new WebButton("Bing Image Search", "Bing");

    btn.addPattern("https://www.bing.com/images/search?q=" + FirstName + "%20" + LastName + "%20" + Field,
        FirstName, LastName);

    btn.addPattern("https://www.bing.com/images/search?q=" + SingleName + "%20" + Field,
        SingleName);

    personImgSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern("https://duckduckgo.com/?q=" + FirstName + "%20" + LastName + "%20" + Field + "&iar=images&iax=images&ia=images",
        FirstName, LastName);

    btn.addPattern("https://duckduckgo.com/?q=" + SingleName + "%20" + Field + "&iar=images&iax=images&ia=images",
        SingleName);

    personImgSrchList.add(btn);

 // Institution search button ----------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "");

    btn.addPattern("http://www.google.com/search?q=" + Name + "%20" + DivisionName,
        Name, DivisionName);

    btn.addPattern("http://www.google.com/search?q=" + Name,
        Name);

    instSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_INST_SRCH, btn);

    btn = new WebButton("Bing", "");

    btn.addPattern("https://www.bing.com/search?q=" + Name + "%20" + DivisionName,
        Name, DivisionName);

    btn.addPattern("https://www.bing.com/search?q=" + Name,
        Name);

    instSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "");

    btn.addPattern("https://duckduckgo.com/?q=" + Name + "%20" + DivisionName,
        Name, DivisionName);

    btn.addPattern("https://duckduckgo.com/?q=" + Name,
        Name);

    instSrchList.add(btn);


 // Institution map search button ------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google Maps", "Google Maps");

    btn.addPattern("https://maps.google.com/maps?q=" + Name + ",+" + City + ",+" + Region + ",+" + Country + "&hl=en",
        Name);

    instMapSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_INST_MAP_SRCH, btn);

    btn = new WebButton("OpenStreetMap", "OpenStreet");

    btn.addPattern("https://www.openstreetmap.org/search?query=" + Name + ",+" + City + ",+" + Region + ",+" + Country,
        Name);

    instMapSrchList.add(btn);

    btn = new WebButton("Bing Maps", "Bing Maps");

    btn.addPattern("http://www.bing.com/maps/default.aspx?where1=" + Name + ',' + City + ',' + Region + ',' + Country,
        Name);

    instMapSrchList.add(btn);

 // DOI search menu command ------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "Google");

    btn.addPattern("http://www.google.com/search?q=doi%3A" + doi,
        doi);

    doiSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_DOI_SRCH, btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern("https://www.bing.com/search?q=doi%3A" + doi,
        doi);

    doiSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern("https://duckduckgo.com/?q=doi%3A" + doi,
        doi);

    doiSrchList.add(btn);

 // ISBN search menu command -----------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("WorldCat", "WorldCat");

    btn.addPattern("http://www.worldcat.org/search?q=bn%3A" + ISBN + "&qt=advanced",
        ISBN);

    isbnSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_ISBN_SRCH, btn);

    btn = new WebButton("Amazon", "Amazon");

    btn.addPattern("https://www.amazon.com/s?&rh=p_66%3A" + ISBN,
        ISBN);

    isbnSrchList.add(btn);

 // Work search buttons ----------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("WorldCat", "WorldCat");

    btn.addPattern("http://www.worldcat.org/search?q=au%3A" + SingleName + "+AND+ti%3A" + Title + "&fq=yr%3A" + NumericYear + ".." + NumericYear + "&qt=advanced&datePublished=" + NumericYear + '-' + NumericYear,
        Title, NumericYear, SingleName);

    btn.addPattern("http://www.worldcat.org/search?q=au%3A" + SingleName + "+AND+ti%3A" + Title + "&qt=advanced",
        Title, SingleName);

    btn.addPattern("http://www.worldcat.org/search?q=ti%3A" + Title + "&fq=yr%3A" + NumericYear + ".." + NumericYear + "&qt=advanced&datePublished=" + NumericYear + '-' + NumericYear,
        Title, NumericYear);

    btn.addPattern("http://www.worldcat.org/search?q=ti%3A" + Title + "&qt=advanced",
        Title);

    btn.addPattern("http://www.worldcat.org/search?q=au%3A" + SingleName + "&fq=yr%3A" + NumericYear + ".." + NumericYear + "&qt=advanced&datePublished=" + NumericYear + '-' + NumericYear,
        NumericYear, SingleName);

    btn.addPattern("http://www.worldcat.org/search?q=au%3A" + SingleName + "&qt=advanced",
        SingleName);

    btn.addPattern("http://www.worldcat.org/search?q=bn%3A" + ISBN + "&qt=advanced",
        ISBN);

    workSrchList.add(btn);
    workSrchDefaults.add(btn);

    btn = new WebButton("Google Scholar", "Scholar");

    btn.addPattern("https://scholar.google.com/scholar?q=author%3A%22" + SingleName + "%22%20intitle%3A%22" + QueryTitle + "%22",
        SingleName, QueryTitle);

    btn.addPattern("https://scholar.google.com/scholar?q=intitle%3A%22" + QueryTitle + "%22",
        QueryTitle);

    workSrchList.add(btn);
    workSrchDefaults.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern("https://philpapers.org/s/@author%20" + SingleName + "%20@title%20%22" + QueryTitle + "%22",
        SingleName, QueryTitle);

    btn.addPattern("https://philpapers.org/s/@title%20%22" + QueryTitle + "%22",
        QueryTitle);

    workSrchList.add(btn);

 // Debate, Position, Argument, and Term search buttons --------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "Google");

    btn.addPattern("http://www.google.com/search?q=" + Name,
        Name);

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Stanford Encyclopedia of Philosophy", "SEP");

    btn.addPattern("http://plato.stanford.edu/search/searcher.py?query=" + Name,
        Name);

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Internet Encyclopedia of Philosophy", "IEP");

    btn.addPattern("https://cse.google.com/cse?cx=001101905209118093242%3Arsrjvdp2op4&ie=UTF-8&q=" + Name + "&sa=Search",
        Name);

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Wikipedia", "Wikipedia");

    btn.addPattern("http://en.wikipedia.org/w/index.php?search=" + Name,
        Name);

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Routledge Encyclopedia of Philosophy", "Routledge");

    btn.addPattern("https://www.rep.routledge.com/search?searchString=" + Name,
        Name);

    genSrchList.add(btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern("https://www.bing.com/search?q=" + Name,
        Name);

    genSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern("https://duckduckgo.com/?q=" + Name,
        Name);

    genSrchList.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern("https://philpapers.org/s/" + Name,
        Name);

    genSrchList.add(btn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    Preferences node = app.prefs.node(PREF_KEY_WEB_BUTTONS);

    webBtnCtrlList.forEach(btnCtrls -> btnCtrls.saveToPrefNode(node));

    HyperTab.updateAllWebButtons(node);

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void loadPrefs()
  {
    populatePresets();

    try
    {
      Preferences node = app.prefs.node(PREF_KEY_WEB_BUTTONS);

      WebButtonTable.loadPref(node, personSrchList, PREF_KEY_PERSON_SRCH, personSrchDefaults);
      WebButtonTable.loadPref(node, workSrchList  , PREF_KEY_WORK_SRCH  , workSrchDefaults  );
      WebButtonTable.loadPref(node, genSrchList   , PREF_KEY_GEN_SRCH   , genSrchDefaults   );

      WebButtonBar.loadPref(node, personImgSrchList, PREF_KEY_PERSON_IMG_SRCH);
      WebButtonBar.loadPref(node, instSrchList     , PREF_KEY_INST_SRCH      );
      WebButtonBar.loadPref(node, instMapSrchList  , PREF_KEY_INST_MAP_SRCH  );
      WebButtonBar.loadPref(node, doiSrchList      , PREF_KEY_DOI_SRCH       );
      WebButtonBar.loadPref(node, isbnSrchList     , PREF_KEY_ISBN_SRCH      );

      HyperTab.updateAllWebButtons(node);
    }
    catch (BackingStoreException e)
    {
      e.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
