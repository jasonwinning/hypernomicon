/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.dialogs;

import java.io.File;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;

import com.github.scribejava.core.exceptions.OAuthException;
import com.github.scribejava.core.model.OAuth1AccessToken;
import com.github.scribejava.core.model.OAuth1RequestToken;
import com.github.scribejava.core.oauth.OAuth10aService;

import org.hypernomicon.bib.lib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroOAuthApi;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.util.CryptoUtil;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;
import javafx.stage.FileChooser;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

//---------------------------------------------------------------------------  

public class OptionsDialogController extends HyperDialog
{ 
  @FXML private TabPane tabPane;
  @FXML private Tab tabLinkToExtBibMgr;
  @FXML private Tab tabUnlinkFromExtBibMgr;
  @FXML private AnchorPane apLinkToExtBibMgr;
  @FXML private AnchorPane apUnlinkFromExtBibMgr;
  @FXML private Label lblCurrentlyLinked;
  @FXML private Button btnUnlink;

  @FXML private CheckBox chkInternet;
  @FXML private CheckBox chkAutoOpenPDF;
  @FXML private CheckBox chkAutoRetrieveBib;
  @FXML private TextField tfImageEditor;
  @FXML private TextField tfPDFReader;
  @FXML private Button btnImageEditorBrowse;
  @FXML private Button btnPDFReaderBrowse;
  @FXML private Slider sliderFontSize;
  @FXML private Tab tabNaming;
  
  @FXML private Button btnAuthorize;
  @FXML private Button btnVerify;
  @FXML private Label lblRedirect;
  @FXML private TextField tfVerificationCode;
  @FXML private Label lblStep2;
  @FXML private Label lblStep2Instructions;
  @FXML private Label lblStep3;
  @FXML private Label lblStep3Instructions;
  @FXML private Label lblStep4;
  @FXML private Label lblStep4Instructions;
  
  @FXML private ComboBox<String> cbComponent1;
  @FXML private ComboBox<String> cbComponent2;
  @FXML private ComboBox<String> cbComponent3;
  @FXML private ComboBox<String> cbComponent4;
  @FXML private ComboBox<String> cbComponent5;
  @FXML private TextField tfSepBefore1;
  @FXML private TextField tfSepBefore2;
  @FXML private TextField tfSepBefore3;
  @FXML private TextField tfSepBefore4;
  @FXML private TextField tfSepBefore5;
  @FXML private TextField tfSepWithin1;
  @FXML private TextField tfSepWithin2;
  @FXML private TextField tfSepWithin3;
  @FXML private TextField tfSepWithin4;
  @FXML private TextField tfSepWithin5;
  @FXML private TextField tfSepAfter1;
  @FXML private TextField tfSepAfter2;
  @FXML private TextField tfSepAfter3;
  @FXML private TextField tfSepAfter4;
  @FXML private TextField tfSepAfter5;
  @FXML private TextField tfTest1;
  @FXML private TextField tfTest2;
  @FXML private TextField tfTest3;
  @FXML private TextField tfTest4;
  @FXML private TextField tfTest5;
  @FXML private CheckBox chkTreatEdAsAuthor;
  @FXML private CheckBox chkAddInitial;
  @FXML private CheckBox chkYearLetter;
  @FXML private CheckBox chkPosix;
  @FXML private CheckBox chkLowercase;
  @FXML private TextField tfMaxChar;
  @FXML private TextField tfExample;
  @FXML private Label lblExample;
  
  private static HashMap<String, Integer> componentMap;
  private StringProperty authUrl;
  private OAuth1RequestToken requestToken;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override protected boolean isValid()          { return true; }  
  @FXML private void btnImageEditorBrowseClick() { browseClick(tfImageEditor); }  
  @FXML private void btnPDFReaderClick()         { browseClick(tfPDFReader); }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void browseClick(TextField tf)
  {   
    FileChooser fileChooser = new FileChooser();

    // Set extension filter
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    fileChooser.setInitialDirectory(new File(System.getProperty("user.dir")));

    // Show save file dialog
    File file = fileChooser.showOpenDialog(dialogStage);

    if (file == null) return;
    
    tf.setText(file.getPath());
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static OptionsDialogController create(String title)
  {
    OptionsDialogController odc = HyperDialog.create("OptionsDialog.fxml", title, true);
    odc.init();
    return odc;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void init()
  {
    btnAuthorize.setOnAction(event -> btnAuthorizeClick());
    lblRedirect.setOnMouseClicked(event -> openWebLink(authUrl.get()));
    btnVerify.setOnAction(event -> btnVerifyClick());
    btnUnlink.setOnAction(event -> btnUnlinkClick());
    
    authUrl = new SimpleStringProperty();
    
    btnAuthorize.disableProperty().bind(authUrl.isNotEmpty());
    lblRedirect.visibleProperty().bind(authUrl.isNotEmpty());
    
    btnVerify.disableProperty().bind(tfVerificationCode.textProperty().isEmpty());
    
    tabPane.getTabs().remove(tabUnlinkFromExtBibMgr);
    
    if (db.isLoaded() == false)
      tabPane.getTabs().remove(tabLinkToExtBibMgr);
    else if (db.bibLibraryIsLinked())
      tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
    
    btnVerify.visibleProperty().bind(authUrl.isNotEmpty());
    lblRedirect.visibleProperty().bind(authUrl.isNotEmpty());
    tfVerificationCode.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep2.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep2Instructions.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep3.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep3Instructions.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep4.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep4Instructions.visibleProperty().bind(authUrl.isNotEmpty());
    
    componentMap = new LinkedHashMap<>();
    componentMap.put("Author last names", AUTHOR_FN_COMPONENT);
    componentMap.put("Year", YEAR_FN_COMPONENT);
    componentMap.put("Title (no subtitle)", TITLE_FN_COMPONENT);
    componentMap.put("Translators", TRANS_FN_COMPONENT);
    componentMap.put("Editors", EDITOR_FN_COMPONENT);
    componentMap.put("", BLANK_FN_COMPONENT);
    
    initAppTextField(tfImageEditor, PREF_KEY_IMAGE_EDITOR);
    initAppTextField(tfPDFReader, PREF_KEY_PDF_READER);
   
    sliderFontSize.setValue(appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE));
    sliderFontSize.valueProperty().addListener((ChangeListener<Number>)(observable, oldValue, newValue) ->
    {
      if (oldValue == null) return;
      if (newValue == null) return;
      if ((oldValue.doubleValue() == newValue.doubleValue())) return;
      if (appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE) == newValue.doubleValue()) return;
      
      appPrefs.putDouble(PREF_KEY_FONT_SIZE, newValue.doubleValue());
    });
    
    sliderFontSize.setTooltip(new Tooltip("Base font size"));
    
    initAppCheckBox(chkInternet, PREF_KEY_CHECK_INTERNET, true);
    initAppCheckBox(chkAutoOpenPDF, PREF_KEY_AUTO_OPEN_PDF, true);
    initAppCheckBox(chkAutoRetrieveBib, PREF_KEY_AUTO_RETRIEVE_BIB, true);
       
    boolean disable = (db == null) || (db.prefs == null) || (db.isLoaded() == false);
    
    tabNaming.setDisable(disable);
   
    if (disable == false)
    {     
      initDBTextField(tfSepWithin1, PREF_KEY_FN_WITHIN_SEP_1);
      initDBTextField(tfSepWithin2, PREF_KEY_FN_WITHIN_SEP_2);
      initDBTextField(tfSepWithin3, PREF_KEY_FN_WITHIN_SEP_3);
      initDBTextField(tfSepWithin4, PREF_KEY_FN_WITHIN_SEP_4);
      initDBTextField(tfSepWithin5, PREF_KEY_FN_WITHIN_SEP_5);

      initDBTextField(tfSepBefore1, PREF_KEY_FN_BEFORE_SEP_1);
      initDBTextField(tfSepBefore2, PREF_KEY_FN_BEFORE_SEP_2);
      initDBTextField(tfSepBefore3, PREF_KEY_FN_BEFORE_SEP_3);
      initDBTextField(tfSepBefore4, PREF_KEY_FN_BEFORE_SEP_4);
      initDBTextField(tfSepBefore5, PREF_KEY_FN_BEFORE_SEP_5);

      initDBTextField(tfSepAfter1, PREF_KEY_FN_AFTER_SEP_1);
      initDBTextField(tfSepAfter2, PREF_KEY_FN_AFTER_SEP_2);
      initDBTextField(tfSepAfter3, PREF_KEY_FN_AFTER_SEP_3);
      initDBTextField(tfSepAfter4, PREF_KEY_FN_AFTER_SEP_4);
      initDBTextField(tfSepAfter5, PREF_KEY_FN_AFTER_SEP_5);

      initDBTextField(tfTest1, PREF_KEY_FN_TEST_1);
      initDBTextField(tfTest2, PREF_KEY_FN_TEST_2);
      initDBTextField(tfTest3, PREF_KEY_FN_TEST_3);
      initDBTextField(tfTest4, PREF_KEY_FN_TEST_4);
      initDBTextField(tfTest5, PREF_KEY_FN_TEST_5);
      
      initDBCheckBox(chkTreatEdAsAuthor, PREF_KEY_FN_TREAT_ED_AS_AUTHOR, true);
      initDBCheckBox(chkAddInitial, PREF_KEY_FN_ADD_INITIAL, false);
      initDBCheckBox(chkYearLetter, PREF_KEY_FN_YEAR_LETTER, false);
      initDBCheckBox(chkPosix, PREF_KEY_FN_POSIX, false);
      initDBCheckBox(chkLowercase, PREF_KEY_FN_LOWERCASE, false);
         
      initMaxChar(tfMaxChar, PREF_KEY_FN_MAX_CHAR);
      
      initComponentCB(cbComponent1, PREF_KEY_FN_COMPONENT_1, AUTHOR_FN_COMPONENT);
      initComponentCB(cbComponent2, PREF_KEY_FN_COMPONENT_2, EDITOR_FN_COMPONENT);
      initComponentCB(cbComponent3, PREF_KEY_FN_COMPONENT_3, TRANS_FN_COMPONENT);
      initComponentCB(cbComponent4, PREF_KEY_FN_COMPONENT_4, YEAR_FN_COMPONENT);
      initComponentCB(cbComponent5, PREF_KEY_FN_COMPONENT_5, TITLE_FN_COMPONENT);
      
      refreshExample();
    }
    
    lblExample.setOnMouseClicked(event -> refreshExample());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   
  
  private void initComponentCB(ComboBox<String> cb, String prefKey, int defValue)
  {
    ObservableList<String> choices = FXCollections.observableArrayList();
    int selNdx = 0, selCode;
    
    choices.addAll(componentMap.keySet());
    
    cb.setItems(null);
    cb.setItems(choices);

    selCode = db.prefs.getInt(prefKey, defValue);
    db.prefs.putInt(prefKey, selCode);
    
    for (Entry<String, Integer> entry : componentMap.entrySet())
      if (entry.getValue() == selCode)
        selNdx = cb.getItems().indexOf(entry.getKey());
    
    cb.getSelectionModel().select(selNdx);
    
    cb.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      db.prefs.putInt(prefKey, componentMap.get(newValue));
      refreshExample();
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  private void initAppTextField(TextField tf, String prefKey)
  {
    tf.setText(appPrefs.get(prefKey, ""));
    
    tf.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      appPrefs.put(prefKey, newValue);
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   
  
  private void initDBCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(db.prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      db.prefs.putBoolean(prefKey, newValue.booleanValue());
      refreshExample();
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  private void initAppCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(appPrefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      appPrefs.putBoolean(prefKey, newValue.booleanValue());
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  private void initMaxChar(TextField tf, String prefKey)
  {
    tf.setText("" + db.prefs.getInt(prefKey, 255));
    
    tf.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      int intVal = parseInt(newValue, -1);
      if (intVal < 1)
        intVal = 255;
      
      if (intVal < 14)
        intVal = 14;        
      
      db.prefs.putInt(prefKey, intVal);
      refreshExample();
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  private void refreshExample()
  {
    String author = "", title = "", year = "", trans = "", editor = "", value;
    int code = BLANK_FN_COMPONENT;
    
    for (int ndx = 1; ndx <= 5; ndx++)
    {
      value = "";
      
      switch (ndx)
      {
        case 1 : 
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_1, BLANK_FN_COMPONENT); 
          value = db.prefs.get(PREF_KEY_FN_TEST_1, "");
          break;

        case 2 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_2, BLANK_FN_COMPONENT); 
          value = db.prefs.get(PREF_KEY_FN_TEST_2, "");
          break;

        case 3 : 
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_3, BLANK_FN_COMPONENT); 
          value = db.prefs.get(PREF_KEY_FN_TEST_3, "");
          break;

        case 4 : 
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_4, BLANK_FN_COMPONENT); 
          value = db.prefs.get(PREF_KEY_FN_TEST_4, "");
          break;
          
        case 5 : 
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_5, BLANK_FN_COMPONENT); 
          value = db.prefs.get(PREF_KEY_FN_TEST_5, "");
          break;
      }
    
      switch (code)
      {
        case AUTHOR_FN_COMPONENT : author = ultraTrim("" + value); break;
        case TITLE_FN_COMPONENT : title = ultraTrim("" + value); break;
        case YEAR_FN_COMPONENT : year = ultraTrim("" + value); break;
        case TRANS_FN_COMPONENT : trans = ultraTrim("" + value); break;
        case EDITOR_FN_COMPONENT : editor = ultraTrim("" + value); break;
      }
    }
    
    ArrayList<FileNameAuthor> authors = new ArrayList<>();
    
    for (String authorStr : author.split(";"))
    {
      String trAuthorStr = ultraTrim(authorStr);
      if (trAuthorStr.length() > 0)
        authors.add(new FileNameAuthor(trAuthorStr, false, false));
    }
      
    for (String transStr : trans.split(";"))
    {
      String trTransStr = ultraTrim(transStr);
      if (trTransStr.length() > 0)
        authors.add(new FileNameAuthor(trTransStr, false, true));
    }
    
    for (String edStr : editor.split(";"))
    {
      String trEdStr = ultraTrim(edStr);
      if (trEdStr.length() > 0)
        authors.add(new FileNameAuthor(trEdStr, true, false));
    }
        
    tfExample.setText(HDT_WorkFile.makeFileName(authors, year, title, "pdf"));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  private void initDBTextField(TextField tf, String prefKey)
  {
    tf.setText(db.prefs.get(prefKey, ""));
    
    tf.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      db.prefs.put(prefKey, newValue);
      refreshExample();
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
 
  private void btnAuthorizeClick()
  {
    try
    {
      @SuppressWarnings("resource")
      OAuth10aService service = ZoteroOAuthApi.service();
      
      requestToken = service.getRequestToken();
      
      authUrl.set(service.getAuthorizationUrl(requestToken));
    }
    catch (UnknownHostException e)
    {
      messageDialog("Unable to connect to host: " + e.getMessage(), mtError);
    }
    catch (IOException | InterruptedException | ExecutionException e)
    {
      messageDialog("Error: " + e.getMessage(), mtError);
    }    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void btnVerifyClick()
  {
    boolean success = false;
    authUrl.set("");
    
    if (tfVerificationCode.textProperty().isEmpty().get())
    {
      messageDialog("You must enter a verification code.", mtWarning);
      safeFocus(tfVerificationCode);
    }
    
    OAuth1AccessToken accessToken = null;
    
    try (OAuth10aService service = ZoteroOAuthApi.service())
    {
      accessToken = service.getAccessToken(requestToken, tfVerificationCode.getText());
      success = true;
    }
    catch (UnknownHostException e)                                    { messageDialog("Unable to connect to host: " + e.getMessage(), mtError);  }
    catch (OAuthException e)                                          { messageDialog("Verification code was rejected by the server.", mtError); }
    catch (IOException | InterruptedException | ExecutionException e) { messageDialog("Verification failed: " + e.getMessage(), mtError); }

    if (success == false) return;
        
    String encApiKey;
    
    try
    {
      encApiKey = CryptoUtil.encrypt("", accessToken.getTokenSecret());
      db.linkBibLibrary(LibraryType.ltZotero, encApiKey, accessToken.getParameter("userID"));
    } 
    catch (Exception e)
    {
      messageDialog("Verification failed: " + e.getMessage(), mtError);
      return;      
    }
    
    SyncBibDialogController.create().sync();
        
    tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void btnUnlinkClick()
  {
    String typeName = db.getBibLibrary().type().getUserReadableName();
    
    if (confirmDialog("Do you really want to unlink your " + typeName + " account?" + System.lineSeparator() + 
                      "All associations between Hypernomicon records and " + typeName + " entries will be erased." + System.lineSeparator() +
                      "This process cannot be undone.") == false)
      return;
    
    db.unlinkBibLibrary();
    tabLinkToExtBibMgr.setContent(apLinkToExtBibMgr);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}