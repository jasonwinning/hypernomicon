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

import java.io.File;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.concurrent.ExecutionException;

import com.github.scribejava.core.exceptions.OAuthException;
import com.github.scribejava.core.model.OAuth1AccessToken;
import com.github.scribejava.core.model.OAuth1RequestToken;
import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth10aService;
import com.github.scribejava.core.oauth.OAuth20Service;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.App;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.mendeley.MendeleyOAuthApi;
import org.hypernomicon.bib.zotero.ZoteroOAuthApi;
import org.hypernomicon.util.CryptoUtil;
import org.hypernomicon.view.dialogs.HyperDlg;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;
import javafx.stage.FileChooser;
import javafx.stage.Window;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

//---------------------------------------------------------------------------

public class SettingsDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apLinkToExtBibMgr, apUnlinkFromExtBibMgr;
  @FXML private ToggleButton btnZoteroAuthorize, btnMendeleyAuthorize, btnComputer, btnDatabase;
  @FXML private Button btnCodePaste, btnUnlink, btnVerify, btnImgEditorAdvanced, btnPdfViewerAdvanced;
  @FXML private CheckBox chkAutoOpenPDF, chkNewVersionCheck, chkAutoRetrieveBib, chkInternet, chkUseSentenceCase, chkLinuxWorkaround;
  @FXML private Label lblCurrentlyLinked, lblRedirect, lblStep2, lblStep2Instructions,
                      lblStep3, lblStep3Instructions, lblStep4, lblStep4Instructions;
  @FXML private Slider sliderFontSize;
  @FXML private Tab tabLinkToExtBibMgr, tabDBSpecific, tabNaming, tabUnlinkFromExtBibMgr, tabWebButtons;
  @FXML private TabPane tpMain, tpComputerSpecific, tpDBSpecific;
  @FXML private TextField tfImageEditor, tfPDFReader, tfVerificationCode;

  private StringProperty authUrl;
  private OAuth1RequestToken requestToken;
  private boolean noDB;
  private SettingsControl webBtnSettingsCtrlr;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnImageEditorBrowseClick() { browseClick(dialogStage, tfImageEditor); }
  @FXML private void btnPDFReaderClick()         { browseClick(dialogStage, tfPDFReader); }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SettingsDlgCtrlr create()
  {
    SettingsDlgCtrlr odc = HyperDlg.createUsingFullPath("view/settings/SettingsDlg.fxml", appTitle + " Settings", true);
    odc.init();
    return odc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    noDB = (db == null) || (db.prefs == null) || (db.isLoaded() == false);

    webBtnSettingsCtrlr = initControl(tabWebButtons, "WebButtonSettings");
    initControl(tabNaming, "WorkFileNamingSettings");

    btnZoteroAuthorize.setOnAction(event -> btnAuthorizeClick(LibraryType.ltZotero));
    btnMendeleyAuthorize.setOnAction(event -> btnAuthorizeClick(LibraryType.ltMendeley));
    lblRedirect.setOnMouseClicked(event -> openWebLink(authUrl.get()));

    btnComputer.selectedProperty().addListener((ob, ov, nv) ->
    {
      tpMain.getSelectionModel().select(nv.booleanValue() ? 0 : 1);
    });

    btnComputer.getToggleGroup().selectedToggleProperty().addListener((ob, oldVal, newVal) ->
    {
      if (newVal == null)
        oldVal.setSelected(true);
    });

    btnVerify.setOnAction(event ->
    {
      if (tfVerificationCode.textProperty().isEmpty().get())
      {
        falseWithWarningMessage("You must enter a verification code.", tfVerificationCode);
        return;
      }

      if (btnZoteroAuthorize.isSelected())
        zoteroVerifyClick();
      else
        mendeleyVerifyClick();
    });

    btnUnlink.setOnAction(event -> btnUnlinkClick());

    authUrl = new SimpleStringProperty();

    btnZoteroAuthorize.disableProperty().bind(authUrl.isNotEmpty());
    btnMendeleyAuthorize.disableProperty().bind(authUrl.isNotEmpty());
    lblRedirect.visibleProperty().bind(authUrl.isNotEmpty());

    btnCodePaste.setOnAction(event -> tfVerificationCode.setText(getClipboardText(true)));
    setToolTip(btnCodePaste, "Paste text from clipboard");

    btnVerify.disableProperty().bind(tfVerificationCode.textProperty().isEmpty());

    tpDBSpecific.getTabs().remove(tabUnlinkFromExtBibMgr);

    if (db.isLoaded() == false)
      tpComputerSpecific.getTabs().remove(tabLinkToExtBibMgr);
    else if (db.bibLibraryIsLinked())
    {
      setUnlinkMessage();
      tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
    }

    btnVerify           .visibleProperty().bind(authUrl.isNotEmpty());
    lblRedirect         .visibleProperty().bind(authUrl.isNotEmpty());
    btnCodePaste        .visibleProperty().bind(authUrl.isNotEmpty());
    tfVerificationCode  .visibleProperty().bind(authUrl.isNotEmpty());
    lblStep2            .visibleProperty().bind(authUrl.isNotEmpty());
    lblStep2Instructions.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep3            .visibleProperty().bind(authUrl.isNotEmpty());
    lblStep3Instructions.visibleProperty().bind(authUrl.isNotEmpty());
    lblStep4            .visibleProperty().bind(authUrl.isNotEmpty());
    lblStep4Instructions.visibleProperty().bind(authUrl.isNotEmpty());

    initAppTextField(tfImageEditor, PREF_KEY_IMAGE_EDITOR);
    initAppTextField(tfPDFReader, PREF_KEY_PDF_READER);

    btnImgEditorAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = LaunchCommandsDlgCtrlr.create
          ("Modify Image Editor Command(s)", PREF_KEY_IMAGE_EDITOR, PREF_KEY_IMAGE_EDITOR_COMMANDS, PREF_KEY_IMAGE_EDITOR_COMMAND_TYPE);

      if (lcdc.showModal())
        tfImageEditor.setText(appPrefs.get(PREF_KEY_IMAGE_EDITOR, ""));
    });

    btnPdfViewerAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = LaunchCommandsDlgCtrlr.create
          ("Modify PDF Viewer Command(s)", PREF_KEY_PDF_READER, PREF_KEY_PDF_READER_COMMANDS, PREF_KEY_PDF_READER_COMMAND_TYPE);

      if (lcdc.showModal())
        tfPDFReader.setText(appPrefs.get(PREF_KEY_PDF_READER, ""));
    });

    sliderFontSize.setValue(appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE));
    sliderFontSize.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((oldValue == null) || (newValue == null)) return;
      if ((oldValue.doubleValue() == newValue.doubleValue())) return;
      if (appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE) == newValue.doubleValue()) return;

      appPrefs.putDouble(PREF_KEY_FONT_SIZE, newValue.doubleValue());
    });

    setToolTip(sliderFontSize, "Base font size");

    initAppCheckBox(chkInternet, PREF_KEY_CHECK_INTERNET, true);
    initAppCheckBox(chkNewVersionCheck, PREF_KEY_CHECK_FOR_NEW_VERSION, true);
    initAppCheckBox(chkAutoOpenPDF, PREF_KEY_AUTO_OPEN_PDF, true);
    initAppCheckBox(chkAutoRetrieveBib, PREF_KEY_AUTO_RETRIEVE_BIB, true);

    chkLinuxWorkaround.setVisible(SystemUtils.IS_OS_LINUX);

    initAppCheckBox(chkLinuxWorkaround, PREF_KEY_LINUX_WORKAROUND, false);

    tabDBSpecific.setDisable(noDB);
    tabNaming.setDisable(noDB);

    if (noDB == false)
      initDBCheckBox(chkUseSentenceCase, PREF_KEY_SENTENCE_CASE, false);

    dialogStage.setOnHiding(event -> webBtnSettingsCtrlr.save());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static interface SettingsControl { void init(boolean noDB); void save(); }

  private SettingsControl initControl(Tab tab, String fxmlName)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("view/settings/" + fxmlName + ".fxml"));
      AnchorPane ap = loader.load();
      tab.setContent(ap);
      SettingsControl ctrlr = loader.getController();
      ctrlr.init(noDB);
      return ctrlr;
    }
    catch (IOException e)
    {
      e.printStackTrace();
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initAppTextField(TextField tf, String prefKey)
  {
    tf.setText(appPrefs.get(prefKey, ""));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        appPrefs.put(prefKey, newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initDBCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(db.prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      db.prefs.putBoolean(prefKey, newValue.booleanValue());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initAppCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(appPrefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        appPrefs.putBoolean(prefKey, newValue.booleanValue());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnAuthorizeClick(LibraryType libType)
  {
    try
    {
      lblStep2Instructions.setText("Click link to open web site where you will authorize Hypernomicon to access your " +
                                   libType.getUserFriendlyName() + " account and get verification code.");

      if (libType == LibraryType.ltZotero)
      {
        try (OAuth10aService service = ZoteroOAuthApi.service())
        {
          requestToken = service.getRequestToken();

          authUrl.set(service.getAuthorizationUrl(requestToken));
        }
      }
      else if (libType == LibraryType.ltMendeley)
      {
        try (OAuth20Service service = MendeleyOAuthApi.service())
        {
          authUrl.set(service.getAuthorizationUrl());
        }
      }
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

  private void mendeleyVerifyClick()
  {
    boolean success = false;

    authUrl.set("");
    btnZoteroAuthorize.setSelected(false);
    btnMendeleyAuthorize.setSelected(false);

    OAuth2AccessToken accessToken = null;

    try (OAuth20Service service = MendeleyOAuthApi.service())
    {
      accessToken = service.getAccessToken(tfVerificationCode.getText());
      success = true;
    }
    catch (UnknownHostException e)                                    { messageDialog("Unable to connect to host: " + e.getMessage(), mtError);  }
    catch (OAuthException e)                                          { messageDialog("Verification code was rejected by the server.", mtError); }
    catch (IOException | InterruptedException | ExecutionException e) { messageDialog("Verification failed: " + e.getMessage(), mtError); }

    if (success == false) return;

    try
    {
      db.linkMendeleyLibrary(CryptoUtil.encrypt("", accessToken.getAccessToken()), CryptoUtil.encrypt("", accessToken.getRefreshToken()));
    }
    catch (Exception e)
    {
      messageDialog("Verification failed: " + e.getMessage(), mtError);
      return;
    }

    SyncBibDlgCtrlr.create().sync();

    setUnlinkMessage();
    tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void zoteroVerifyClick()
  {
    boolean success = false;

    authUrl.set("");
    btnZoteroAuthorize.setSelected(false);
    btnMendeleyAuthorize.setSelected(false);
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

    try
    {
      db.linkZoteroLibrary(CryptoUtil.encrypt("", accessToken.getTokenSecret()), accessToken.getParameter("userID"));
    }
    catch (Exception e)
    {
      messageDialog("Verification failed: " + e.getMessage(), mtError);
      return;
    }

    SyncBibDlgCtrlr.create().sync();

    setUnlinkMessage();
    tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setUnlinkMessage()
  {
    lblCurrentlyLinked.setText("This database is currently linked to a " + db.getBibLibrary().type().getUserFriendlyName() + " library.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnUnlinkClick()
  {
    String typeName = db.getBibLibrary().type().getUserFriendlyName();

    if (confirmDialog("Do you really want to unlink your " + typeName + " account?" + System.lineSeparator() +
                      "All associations between Hypernomicon records and " + typeName + " entries will be erased." + System.lineSeparator() +
                      "This process cannot be undone.") == false)
      return;

    db.unlinkBibLibrary();
    tabLinkToExtBibMgr.setContent(apLinkToExtBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void browseClick(Window owner, TextField tf)
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    fileChooser.setInitialDirectory(new File(System.getProperty("user.dir")));

    nullSwitch(ui.windows.showOpenDialog(fileChooser, owner), filePath -> tf.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}