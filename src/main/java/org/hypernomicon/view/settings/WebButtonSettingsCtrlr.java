/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.settings;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.App.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.prefs.Preferences;

import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.view.tabs.HyperTab;

import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.util.StringConverter;

public class WebButtonSettingsCtrlr implements SettingsControl
{
  @FXML private Button btnPersonSrch1Advanced, btnPersonSrch2Advanced, btnPersonImgSrchAdvanced, btnInstSrchAdvanced, btnInstMapSrchAdvanced,
                       btnDOISrchAdvanced, btnISBNSrchAdvanced, btnWorkSrch1Advanced, btnWorkSrch2Advanced,
                       btnGenSrch1Advanced, btnGenSrch2Advanced, btnGenSrch3Advanced, btnGenSrch4Advanced;
  @FXML private ComboBox<WebButton> cbPersonSrch1, cbPersonSrch2, cbPersonImgSrch, cbInstSrch, cbInstMapSrch, cbDOISrch, cbISBNSrch,
                                    cbWorkSrch1, cbWorkSrch2, cbGenSrch1, cbGenSrch2, cbGenSrch3, cbGenSrch4;
  @FXML private TextField tfPersonSrch1, tfPersonSrch2, tfPersonImgSrch, tfWorkSrch1, tfWorkSrch2, tfDOISrch, tfISBNSrch,
                          tfInstMapSrch, tfGenSrch1, tfGenSrch2, tfGenSrch3, tfGenSrch4;


  private final List<ButtonControls> webBtnList = new ArrayList<>();

  private static final List<WebButton> personSrchList    = new ArrayList<>(),
                                       personImgSrchList = new ArrayList<>(),
                                       instSrchList      = new ArrayList<>(),
                                       instMapSrchList   = new ArrayList<>(),
                                       doiSrchList       = new ArrayList<>(),
                                       isbnSrchList      = new ArrayList<>(),
                                       workSrchList      = new ArrayList<>(),
                                       genSrchList       = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class ButtonControls
  {
    public final String prefKey;
    public final TextField tfCaption;
    public final ComboBox<WebButton> cbPreset;
    public final Button btnAdvanced;

    public ButtonControls(String prefKey, TextField tfCaption, ComboBox<WebButton> cbPreset, Button btnAdvanced)
    {
      this.prefKey = prefKey;
      this.tfCaption = tfCaption;
      this.cbPreset = cbPreset;
      this.btnAdvanced = btnAdvanced;

      cbPreset.setConverter(new StringConverter<>()
      {
        @Override public String toString(WebButton btn)
        {
          return btn == null ? "" : btn.name;
        }

        @Override public WebButton fromString(String str)
        {
          for (WebButton btn : cbPreset.getItems())
            if (btn.name.equalsIgnoreCase(str))
              return btn;

          return null;
        }
      });

      cbPreset.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) ->
      {
        if (tfCaption == null) return;
        tfCaption.setText(nv == null ? "" : nv.getCaption());
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    webBtnList.add(new ButtonControls(PREF_KEY_PERSON_SRCH_1  , tfPersonSrch1  , cbPersonSrch1  , btnPersonSrch1Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_PERSON_SRCH_2  , tfPersonSrch2  , cbPersonSrch2  , btnPersonSrch2Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_PERSON_IMG_SRCH, tfPersonImgSrch, cbPersonImgSrch, btnPersonImgSrchAdvanced));
    webBtnList.add(new ButtonControls(PREF_KEY_INST_SRCH      , null           , cbInstSrch     , btnInstSrchAdvanced));
    webBtnList.add(new ButtonControls(PREF_KEY_INST_MAP_SRCH  , tfInstMapSrch  , cbInstMapSrch  , btnInstMapSrchAdvanced));
    webBtnList.add(new ButtonControls(PREF_KEY_DOI_SRCH       , tfDOISrch      , cbDOISrch      , btnDOISrchAdvanced));
    webBtnList.add(new ButtonControls(PREF_KEY_ISBN_SRCH      , tfISBNSrch     , cbISBNSrch     , btnISBNSrchAdvanced));
    webBtnList.add(new ButtonControls(PREF_KEY_WORK_SRCH_1    , tfWorkSrch1    , cbWorkSrch1    , btnWorkSrch1Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_WORK_SRCH_2    , tfWorkSrch2    , cbWorkSrch2    , btnWorkSrch2Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_GEN_SRCH_1     , tfGenSrch1     , cbGenSrch1     , btnGenSrch1Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_GEN_SRCH_2     , tfGenSrch2     , cbGenSrch2     , btnGenSrch2Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_GEN_SRCH_3     , tfGenSrch3     , cbGenSrch3     , btnGenSrch3Advanced));
    webBtnList.add(new ButtonControls(PREF_KEY_GEN_SRCH_4     , tfGenSrch4     , cbGenSrch4     , btnGenSrch4Advanced));

    populatePresetControls();

    webBtnList.forEach(btnCtrls -> btnCtrls.cbPreset.getSelectionModel().select(ui.webButtonMap.get(btnCtrls.prefKey)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populatePresetControls()
  {
    cbPersonSrch1  .setItems(FXCollections.observableArrayList(personSrchList   ));
    cbPersonSrch2  .setItems(FXCollections.observableArrayList(personSrchList   ));
    cbPersonImgSrch.setItems(FXCollections.observableArrayList(personImgSrchList));
    cbInstSrch     .setItems(FXCollections.observableArrayList(instSrchList     ));
    cbInstMapSrch  .setItems(FXCollections.observableArrayList(instMapSrchList  ));
    cbDOISrch      .setItems(FXCollections.observableArrayList(doiSrchList      ));
    cbISBNSrch     .setItems(FXCollections.observableArrayList(isbnSrchList     ));
    cbWorkSrch1    .setItems(FXCollections.observableArrayList(workSrchList     ));
    cbWorkSrch2    .setItems(FXCollections.observableArrayList(workSrchList     ));
    cbGenSrch1     .setItems(FXCollections.observableArrayList(genSrchList      ));
    cbGenSrch2     .setItems(FXCollections.observableArrayList(genSrchList      ));
    cbGenSrch3     .setItems(FXCollections.observableArrayList(genSrchList      ));
    cbGenSrch4     .setItems(FXCollections.observableArrayList(genSrchList      ));
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
    ui.webButtonMap.put(PREF_KEY_PERSON_SRCH_1, btn);

    btn = new WebButton("Scholar", "Scholar");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryName, WebButtonField.LastName),
        "https://scholar.google.com/scholar?q=author:%22" + WebButtonField.QueryName.key + "%20" + WebButtonField.LastName.key + "%22"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName),
        "https://scholar.google.com/scholar?q=author:%22" + WebButtonField.SingleName.key + "%22"));

    personSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_PERSON_SRCH_2, btn);

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
    ui.webButtonMap.put(PREF_KEY_WORK_SRCH_1, btn);

    btn = new WebButton("Google Scholar", "Scholar");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.SingleName, WebButtonField.QueryTitle),
        "https://scholar.google.com/scholar?q=author%3A%22" + WebButtonField.SingleName.key + "%22%20intitle%3A%22" + WebButtonField.QueryTitle.key + "%22"));

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.QueryTitle),
        "https://scholar.google.com/scholar?q=intitle%3A%22" + WebButtonField.QueryTitle.key + "%22"));

    workSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_WORK_SRCH_2, btn);

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
    ui.webButtonMap.put(PREF_KEY_GEN_SRCH_1, btn);

    btn = new WebButton("Stanford Encyclopedia of Philosophy", "SEP");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://plato.stanford.edu/search/searcher.py?query=" + WebButtonField.Name.key));

    genSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_GEN_SRCH_2, btn);

    btn = new WebButton("Internet Encyclopedia of Philosophy", "IEP");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "https://cse.google.com/cse?cx=001101905209118093242%3Arsrjvdp2op4&ie=UTF-8&q=" + WebButtonField.Name.key + "&sa=Search"));

    genSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_GEN_SRCH_3, btn);

    btn = new WebButton("Wikipedia", "Wikipedia");

    btn.addPattern(new UrlPattern(EnumSet.of(WebButtonField.Name),
        "http://en.wikipedia.org/w/index.php?search=" + WebButtonField.Name.key));

    genSrchList.add(btn);
    ui.webButtonMap.put(PREF_KEY_GEN_SRCH_4, btn);

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
    Preferences node = appPrefs.node("webButtons");

    webBtnList.forEach(btnCtrls ->
    {
      WebButton webBtn = btnCtrls.cbPreset.getValue();
      if (btnCtrls.tfCaption != null)
        webBtn.setCaption(btnCtrls.tfCaption.getText());

      ui.webButtonMap.put(btnCtrls.prefKey, webBtn);

      node.put(btnCtrls.prefKey, webBtn.name);

      appPrefs.node("webButtonCaptions").put(btnCtrls.prefKey, webBtn.getCaption());
    });

    HyperTab.updateAllWebButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void loadPrefs()
  {
    populatePresets();

    Preferences node = appPrefs.node("webButtons");

    loadPref(node, personSrchList   , PREF_KEY_PERSON_SRCH_1  );
    loadPref(node, personSrchList   , PREF_KEY_PERSON_SRCH_2  );
    loadPref(node, personImgSrchList, PREF_KEY_PERSON_IMG_SRCH);
    loadPref(node, instSrchList     , PREF_KEY_INST_SRCH      );
    loadPref(node, instMapSrchList  , PREF_KEY_INST_MAP_SRCH  );
    loadPref(node, doiSrchList      , PREF_KEY_DOI_SRCH       );
    loadPref(node, isbnSrchList     , PREF_KEY_ISBN_SRCH      );
    loadPref(node, workSrchList     , PREF_KEY_WORK_SRCH_1    );
    loadPref(node, workSrchList     , PREF_KEY_WORK_SRCH_2    );
    loadPref(node, genSrchList      , PREF_KEY_GEN_SRCH_1     );
    loadPref(node, genSrchList      , PREF_KEY_GEN_SRCH_2     );
    loadPref(node, genSrchList      , PREF_KEY_GEN_SRCH_3     );
    loadPref(node, genSrchList      , PREF_KEY_GEN_SRCH_4     );

    HyperTab.updateAllWebButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void loadPref(Preferences node, List<WebButton> srchList, String prefKey)
  {
    String name = node.get(prefKey, ""),
           caption = appPrefs.node("webButtonCaptions").get(prefKey, "");

    if (name.length() == 0) return;

    for (WebButton btn : srchList)
    {
      if (btn.name.equals(name))
      {
        if (caption.length() > 0)
          btn.setCaption(caption);

        ui.webButtonMap.put(prefKey, btn);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
