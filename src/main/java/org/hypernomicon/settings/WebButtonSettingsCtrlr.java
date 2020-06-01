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

package org.hypernomicon.settings;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.App.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;
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
    webBtnCtrlList.add(new WebButtonTable(PREF_KEY_PERSON_SRCH, personSrchList, personSrchDefaults, tvPersonSrch));
    webBtnCtrlList.add(new WebButtonTable(PREF_KEY_WORK_SRCH  , workSrchList,   workSrchDefaults,   tvWorkSrch));
    webBtnCtrlList.add(new WebButtonTable(PREF_KEY_GEN_SRCH   , genSrchList,    genSrchDefaults,    tvGenSrch));

    webBtnCtrlList.add(new WebButtonBar(PREF_KEY_PERSON_IMG_SRCH, personImgSrchList, tfPersonImgSrch, cbPersonImgSrch, btnPersonImgSrchAdvanced));
    webBtnCtrlList.add(new WebButtonBar(PREF_KEY_INST_SRCH      , instSrchList,      null           , cbInstSrch     , btnInstSrchAdvanced));
    webBtnCtrlList.add(new WebButtonBar(PREF_KEY_INST_MAP_SRCH  , instMapSrchList,   tfInstMapSrch  , cbInstMapSrch  , btnInstMapSrchAdvanced));
    webBtnCtrlList.add(new WebButtonBar(PREF_KEY_DOI_SRCH       , doiSrchList,       tfDOISrch      , cbDOISrch      , btnDOISrchAdvanced));
    webBtnCtrlList.add(new WebButtonBar(PREF_KEY_ISBN_SRCH      , isbnSrchList,      tfISBNSrch     , cbISBNSrch     , btnISBNSrchAdvanced));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void populatePresets()
  {

 // Person search buttons --------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    WebButton btn = new WebButton("Google", "Google");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "http://www.google.com/search?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "http://www.google.com/search?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key));

    personSrchList.add(btn);
    personSrchDefaults.add(btn);

    btn = new WebButton("Scholar", "Scholar");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryName, WebButtonField.LastName),
        "https://scholar.google.com/scholar?q=author:%22" + WebButtonField.QueryName.key + "%20" + WebButtonField.LastName.key + "%22"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://scholar.google.com/scholar?q=author:%22" + WebButtonField.SingleName.key + "%22"));

    personSrchList.add(btn);
    personSrchDefaults.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryName, WebButtonField.LastName),
        "https://philpapers.org/s/@author%20" + WebButtonField.QueryName.key + "%20" + WebButtonField.LastName.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://philpapers.org/s/@author%20" + WebButtonField.SingleName.key));

    personSrchList.add(btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "https://www.bing.com/search?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://www.bing.com/search?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key));

    personSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "https://duckduckgo.com/?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://duckduckgo.com/?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key));

    personSrchList.add(btn);

 // Person image search button ---------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google Image Search", "Google");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "http://www.google.com/search?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key + "&tbm=isch"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "http://www.google.com/search?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key + "&tbm=isch"));

    personImgSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_PERSON_IMG_SRCH, btn);

    btn = new WebButton("Bing Image Search", "Bing");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "https://www.bing.com/images/search?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://www.bing.com/images/search?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key));

    personImgSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.FirstName, WebButtonField.LastName),
        "https://duckduckgo.com/?q=" + WebButtonField.FirstName.key + "%20" + WebButtonField.LastName.key + "%20" + WebButtonField.Field.key + "&iar=images&iax=images&ia=images"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://duckduckgo.com/?q=" + WebButtonField.SingleName.key + "%20" + WebButtonField.Field.key + "&iar=images&iax=images&ia=images"));

    personImgSrchList.add(btn);

 // Institution search button ----------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name, WebButtonField.DivisionName),
        "http://www.google.com/search?q=" + WebButtonField.Name.key + "%20" + WebButtonField.DivisionName.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://www.google.com/search?q=" + WebButtonField.Name.key));

    instSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_INST_SRCH, btn);

    btn = new WebButton("Bing", "");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name, WebButtonField.DivisionName),
        "https://www.bing.com/search?q=" + WebButtonField.Name.key + "%20" + WebButtonField.DivisionName.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://www.bing.com/search?q=" + WebButtonField.Name.key));

    instSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name, WebButtonField.DivisionName),
        "https://duckduckgo.com/?q=" + WebButtonField.Name.key + "%20" + WebButtonField.DivisionName.key));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://duckduckgo.com/?q=" + WebButtonField.Name.key));

    instSrchList.add(btn);


 // Institution map search button ------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google Maps", "Google Maps");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://maps.google.com/maps?q=" + WebButtonField.Name.key + ",+" +
        WebButtonField.City.key + ",+" + WebButtonField.Region.key + ",+" +
        WebButtonField.Country.key + "&hl=en"));

    instMapSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_INST_MAP_SRCH, btn);

    btn = new WebButton("OpenStreetMap", "OpenStreet");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://www.openstreetmap.org/search?query=" + WebButtonField.Name.key + ",+" +
        WebButtonField.City.key + ",+" + WebButtonField.Region.key + ",+" +
        WebButtonField.Country.key));

    instMapSrchList.add(btn);

    btn = new WebButton("Bing Maps", "Bing Maps");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://www.bing.com/maps/default.aspx?where1=" + WebButtonField.Name.key + "," +
        WebButtonField.City.key + "," + WebButtonField.Region.key + "," +
        WebButtonField.Country.key));

    instMapSrchList.add(btn);

 // DOI search menu command ------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "Google");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.doi),
        "http://www.google.com/search?q=doi%3A" + WebButtonField.doi.key));

    doiSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_DOI_SRCH, btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.doi),
        "https://www.bing.com/search?q=doi%3A" + WebButtonField.doi.key));

    doiSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.doi),
        "https://duckduckgo.com/?q=doi%3A" + WebButtonField.doi.key));

    doiSrchList.add(btn);

 // ISBN search menu command -----------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("WorldCat", "WorldCat");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.ISBN),
        "http://www.worldcat.org/search?q=bn%3A" + WebButtonField.ISBN.key + "&qt=advanced"));

    isbnSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_ISBN_SRCH, btn);

    btn = new WebButton("Amazon", "Amazon");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.ISBN),
        "https://www.amazon.com/s?&rh=p_66%3A" + WebButtonField.ISBN.key));

    isbnSrchList.add(btn);

 // Work search buttons ----------------------------------------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("WorldCat", "WorldCat");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Title, WebButtonField.NumericYear, WebButtonField.SingleName),
        "http://www.worldcat.org/search?q=au%3A" + WebButtonField.SingleName.key + "+ti%3A" + WebButtonField.Title.key + "&fq=yr%3A" + WebButtonField.NumericYear.key + ".." + WebButtonField.NumericYear.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Title, WebButtonField.SingleName),
        "http://www.worldcat.org/search?q=au%3A" + WebButtonField.SingleName.key + "+ti%3A" + WebButtonField.Title.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Title, WebButtonField.NumericYear),
        "http://www.worldcat.org/search?q=ti%3A" + WebButtonField.Title.key + "&fq=yr%3A" + WebButtonField.NumericYear.key + ".." + WebButtonField.NumericYear.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Title),
        "http://www.worldcat.org/search?q=ti%3A" + WebButtonField.Title.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.NumericYear, WebButtonField.SingleName),
        "http://www.worldcat.org/search?q=au%3A" + WebButtonField.SingleName.key + "&fq=yr%3A" + WebButtonField.NumericYear.key + ".." + WebButtonField.NumericYear.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "http://www.worldcat.org/search?q=au%3A" + WebButtonField.SingleName.key + "&qt=advanced"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.ISBN),
        "http://www.worldcat.org/search?q=bn%3A" + WebButtonField.ISBN.key + "&qt=advanced"));

    workSrchList.add(btn);
    workSrchDefaults.add(btn);

    btn = new WebButton("Google Scholar", "Scholar");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName, WebButtonField.QueryTitle),
        "https://scholar.google.com/scholar?q=author%3A%22" + WebButtonField.SingleName.key + "%22%20intitle%3A%22" + WebButtonField.QueryTitle.key + "%22"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryTitle),
        "https://scholar.google.com/scholar?q=intitle%3A%22" + WebButtonField.QueryTitle.key + "%22"));

    workSrchList.add(btn);
    workSrchDefaults.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName, WebButtonField.QueryTitle),
        "https://philpapers.org/s/@author%20" + WebButtonField.SingleName.key + "%20@title%20%22" + WebButtonField.QueryTitle.key + "%22"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryTitle),
        "https://philpapers.org/s/@title%20%22" + WebButtonField.QueryTitle.key + "%22"));

    workSrchList.add(btn);

 // Debate, Position, Argument, and Term search buttons --------------------------------------------------------------
 // ------------------------------------------------------------------------------------------------------------------

    btn = new WebButton("Google", "Google");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://www.google.com/search?q=" + WebButtonField.Name.key));

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Stanford Encyclopedia of Philosophy", "SEP");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://plato.stanford.edu/search/searcher.py?query=" + WebButtonField.Name.key));

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Internet Encyclopedia of Philosophy", "IEP");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://cse.google.com/cse?cx=001101905209118093242%3Arsrjvdp2op4&ie=UTF-8&q=" + WebButtonField.Name.key + "&sa=Search"));

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Wikipedia", "Wikipedia");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://en.wikipedia.org/w/index.php?search=" + WebButtonField.Name.key));

    genSrchList.add(btn);
    genSrchDefaults.add(btn);

    btn = new WebButton("Routledge Encyclopedia of Philosophy", "Routledge");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://www.rep.routledge.com/search?searchString=" + WebButtonField.Name.key));

    genSrchList.add(btn);

    btn = new WebButton("Bing", "Bing");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://www.bing.com/search?q=" + WebButtonField.Name.key));

    genSrchList.add(btn);

    btn = new WebButton("DuckDuckGo", "DuckDuckGo");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://duckduckgo.com/?q=" + WebButtonField.Name.key));

    genSrchList.add(btn);

    btn = new WebButton("PhilPapers", "PhilPapers");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://philpapers.org/s/" + WebButtonField.Name.key));

    genSrchList.add(btn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save()
  {
    Preferences node = appPrefs.node(PREF_KEY_WEB_BUTTONS);

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
      Preferences node = appPrefs.node(PREF_KEY_WEB_BUTTONS);

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
