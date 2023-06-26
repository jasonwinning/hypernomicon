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

import java.io.File;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;

import com.github.scribejava.core.exceptions.OAuthException;
import com.github.scribejava.core.model.OAuth1AccessToken;
import com.github.scribejava.core.model.OAuth1RequestToken;
import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth10aService;
import com.github.scribejava.core.oauth.OAuth20Service;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.App;
import org.hypernomicon.bib.BibCollection;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.mendeley.MendeleyOAuthApi;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;
import org.hypernomicon.bib.zotero.ZoteroOAuthApi;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.CryptoUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Window;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.settings.SettingsDlgCtrlr.SettingsPage.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

//---------------------------------------------------------------------------

public class SettingsDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apLinkToExtBibMgr, apUnlinkFromExtBibMgr;
  @FXML private ToggleButton btnZoteroAuthorize, btnMendeleyAuthorize;
  @FXML private Button btnCodePaste, btnUnlink, btnVerify, btnImgEditorAdvanced, btnPdfViewerAdvanced, btnClearExtPath, btnExtFilesHelp;
  @FXML private CheckBox chkAutoOpenPDF, chkNewVersionCheck, chkAutoRetrieveBib, chkInternet, chkUseSentenceCase, chkLowerCaseTargetNames, chkDefaultChapterWorkType, chkLinuxWorkaround,
                         chkCompDontExpandKeyWorks, chkDBDontExpandKeyWorks;
  @FXML private ComboBox<HyperTableCell> cbDefaultChapterWorkType;
  @FXML private Label lblCurrentlyLinked, lblRedirect, lblStep2, lblStep2Instructions,
                      lblStep3, lblStep3Instructions, lblStep4, lblStep4Instructions;
  @FXML private Slider sliderFontSize;
  @FXML private Tab tabLinkToExtBibMgr, tabComputerSpecific, tabDBSpecific, tabFolders, tabNaming, tabUnlinkFromExtBibMgr, tabWebButtons;
  @FXML TreeView<SettingsPage> treeView;
  @FXML private TabPane tpMain;
  @FXML private TextField tfImageEditor, tfPDFReader, tfExtFiles, tfVerificationCode;

  private final HyperCB hcbDefaultChapterWorkType;
  private final StringProperty authUrl;
  private final boolean noDB;
  private final Set<SettingsControl> settingsCtrlrs;
  private final Map<SettingsPage, Tab> pageToTab = new EnumMap<>(SettingsPage.class);
  private final Map<SettingsPage, TreeItem<SettingsPage>> pageToTreeItem = new EnumMap<>(SettingsPage.class);

  private OAuth1RequestToken requestToken;

  public enum SettingsPage
  {
    CompGeneral("Settings applying to this computer"),
    WebSearch("Web Search Buttons"),
    DBSpecific("Settings applying to this database"),
    Folders("Default Folders"),
    WorkNaming("Work File Naming"),
    BibMgr("Bibliography Manager");

    private final String caption;

    SettingsPage(String caption) { this.caption = caption; }
    @Override public String toString()   { return caption; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

  @FXML private void btnImageEditorBrowseClick() { browseClick(dialogStage, tfImageEditor); }
  @FXML private void btnPDFReaderClick()         { browseClick(dialogStage, tfPDFReader); }
  @FXML private void btnClearExtPathClick()      { tfExtFiles.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SettingsDlgCtrlr()
  {
    this(CompGeneral);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SettingsDlgCtrlr(SettingsPage page)
  {
    super("settings/SettingsDlg", appTitle + " Settings", true, true);

    noDB = (db.prefs == null) || (db.isLoaded() == false);

    initTree();

    setExtFileTooltip();

    settingsCtrlrs = Set.of
    (
      initControl(tabWebButtons, "WebButtonSettings"     ),
      initControl(tabFolders   , "FolderSettings"        ),
      initControl(tabNaming    , "WorkFileNamingSettings")
    );

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

    btnZoteroAuthorize.setOnAction(event -> btnAuthorizeClick(LibraryType.ltZotero));
    btnMendeleyAuthorize.setOnAction(event -> btnAuthorizeClick(LibraryType.ltMendeley));
    lblRedirect.setOnMouseClicked(event -> openWebLink(authUrl.get()));

    btnCodePaste.setOnAction(event -> tfVerificationCode.setText(getClipboardText(true)));
    setToolTip(btnCodePaste, "Paste text from clipboard");

    btnVerify.disableProperty().bind(tfVerificationCode.textProperty().isEmpty());

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
    initAppTextField(tfExtFiles, PREF_KEY_EXT_FILES_1);

    btnImgEditorAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = new LaunchCommandsDlgCtrlr
        ("Modify Image Editor Command(s)", PREF_KEY_IMAGE_EDITOR, PREF_KEY_IMAGE_EDITOR_COMMANDS, PREF_KEY_IMAGE_EDITOR_COMMAND_TYPE);

      if (lcdc.showModal())
        tfImageEditor.setText(app.prefs.get(PREF_KEY_IMAGE_EDITOR, ""));
    });

    btnPdfViewerAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = new LaunchCommandsDlgCtrlr
        ("Modify PDF Viewer Command(s)", PREF_KEY_PDF_READER, PREF_KEY_PDF_READER_COMMANDS, PREF_KEY_PDF_READER_COMMAND_TYPE);

      if (lcdc.showModal())
        tfPDFReader.setText(app.prefs.get(PREF_KEY_PDF_READER, ""));
    });

    setToolTip(btnImgEditorAdvanced, "Open dialog to customize how the image editor is launched");
    setToolTip(btnPdfViewerAdvanced, "Open dialog to customize how the PDF viewer is launched");

    sliderFontSize.setValue(app.prefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE));
    sliderFontSize.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((oldValue == null) || (newValue == null) || (oldValue.doubleValue() == newValue.doubleValue())) return;
      if (app.prefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE) == newValue.doubleValue()) return;

      app.prefs.putDouble(PREF_KEY_FONT_SIZE, newValue.doubleValue());
    });

    setToolTip(sliderFontSize, "Base font size");

    initAppCheckBox(chkInternet              , PREF_KEY_CHECK_INTERNET           , true );
    initAppCheckBox(chkNewVersionCheck       , PREF_KEY_CHECK_FOR_NEW_VERSION    , true );
    initAppCheckBox(chkAutoOpenPDF           , PREF_KEY_AUTO_OPEN_PDF            , true );
    initAppCheckBox(chkAutoRetrieveBib       , PREF_KEY_AUTO_RETRIEVE_BIB        , true );
    initAppCheckBox(chkCompDontExpandKeyWorks, PREF_KEY_DONT_OPEN_EMPTY_KEY_WORKS, false);

    chkLinuxWorkaround.setVisible(SystemUtils.IS_OS_LINUX);

    initAppCheckBox(chkLinuxWorkaround, PREF_KEY_LINUX_WORKAROUND, false);

    disableAllIff(noDB, tabDBSpecific, tabFolders, tabNaming);

    hcbDefaultChapterWorkType = new HyperCB(cbDefaultChapterWorkType, ctDropDownList, new StandardPopulator(hdtWorkType));

    cbDefaultChapterWorkType.disableProperty().bind(chkDefaultChapterWorkType.selectedProperty().not());

    if (noDB == false)
    {
      initDBCheckBox(chkUseSentenceCase     , PREF_KEY_SENTENCE_CASE            , false);
      initDBCheckBox(chkLowerCaseTargetNames, PREF_KEY_LOWER_CASE_TARGET_NAMES  , false);
      initDBCheckBox(chkDBDontExpandKeyWorks, PREF_KEY_DONT_OPEN_EMPTY_KEY_WORKS, false);
      initDefaultChapterWorkType();
    }

    dialogStage.setOnHiding(event -> settingsCtrlrs.forEach(ctrlr -> ctrlr.save(noDB)));

    onShown = () -> treeView.getSelectionModel().select(pageToTreeItem.get(page));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addTreeItem(SettingsPage page, Tab tab, SettingsPage parent)
  {
    pageToTab.put(page, tab);
    TreeItem<SettingsPage> item = new TreeItem<>(page);
    item.setExpanded(true);
    pageToTreeItem.put(page, item);

    if (parent != null)
      pageToTreeItem.get(parent).getChildren().add(item);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initTree()
  {
    TreeItem<SettingsPage> root = new TreeItem<>(null);
    treeView.setRoot(root);

    addTreeItem(CompGeneral, tabComputerSpecific, null       );
    addTreeItem(DBSpecific , tabDBSpecific      , null       );
    addTreeItem(WebSearch  , tabWebButtons      , CompGeneral);
    addTreeItem(Folders    , tabFolders         , DBSpecific );
    addTreeItem(WorkNaming , tabNaming          , DBSpecific );
    addTreeItem(BibMgr     , tabLinkToExtBibMgr , DBSpecific );

    root.getChildren().add(pageToTreeItem.get(CompGeneral));

    if (db.isLoaded()) root.getChildren().add(pageToTreeItem.get(DBSpecific));

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        tpMain.getSelectionModel().select(pageToTab.get(newValue.getValue()));
    });

    tabUnlinkFromExtBibMgr.setContent(null); // apUnlinkFromExtBibMgr has to be removed from this tab before it can be added to other tab

    if (db.isLoaded() && db.bibLibraryIsLinked())
    {
      setUnlinkMessage();
      tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  interface SettingsControl { void init(Window owner, boolean noDB); void save(boolean noDB); }

  private SettingsControl initControl(Tab tab, String fxmlName)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("settings/" + fxmlName + ".fxml"));
      AnchorPane ap = loader.load();
      tab.setContent(ap);
      SettingsControl ctrlr = loader.getController();
      ctrlr.init(dialogStage, noDB);
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

  private static void initAppTextField(TextField tf, String prefKey)
  {
    tf.setText(app.prefs.get(prefKey, ""));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        app.prefs.put(prefKey, newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initDBCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(db.prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      db.prefs.putBoolean(prefKey, newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initDefaultChapterWorkType()
  {
    HDT_WorkType workType = db.workTypes.getByID(db.prefs.getInt(PREF_KEY_DEFAULT_CHAPTER_WORK_TYPE_ID, -1));

    chkDefaultChapterWorkType.setSelected(workType != null);

    if (workType != null)
      hcbDefaultChapterWorkType.selectID(workType.getID());

    chkDefaultChapterWorkType.selectedProperty().addListener((ob, ov, nv) ->
    {
      if (nv == null) return;

      if (Boolean.FALSE.equals(nv))
      {
        db.prefs.putInt(PREF_KEY_DEFAULT_CHAPTER_WORK_TYPE_ID, -1);
        return;
      }

      db.prefs.putInt(PREF_KEY_DEFAULT_CHAPTER_WORK_TYPE_ID, hcbDefaultChapterWorkType.selectedID());
    });

    hcbDefaultChapterWorkType.addListener((ov, nv) ->
    {
      int workTypeID = nullSwitch(HyperTableCell.getRecord(nv), -1, HDT_Record::getID);
      db.prefs.putInt(PREF_KEY_DEFAULT_CHAPTER_WORK_TYPE_ID, workTypeID);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initAppCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(app.prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        app.prefs.putBoolean(prefKey, newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnExtFileBrowseClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath startPath = new FilePath(tfExtFiles.getText());

    if (FilePath.isEmpty(startPath))
    {
      if (db.isLoaded())
      {
        startPath = db.getRootPath();
        FilePath parentPath = startPath.getParent();
        if (FilePath.isEmpty(parentPath) == false)
          startPath = parentPath;
      }
      else
        startPath = new FilePath(userWorkingDir());
    }

    dirChooser.setInitialDirectory(startPath.toFile());
    dirChooser.setTitle("Select Folder");

    nullSwitch(ui.windows.showDirDialog(dirChooser, dialogStage), filePath -> tfExtFiles.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnAuthorizeClick(LibraryType libType)
  {
    try
    {
      lblStep2Instructions.setText("Click link to open web site where you will authorize " + appTitle + " to access your " +
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

    String verificationCode = tfVerificationCode.getText();
    tfVerificationCode.clear();

    try (OAuth20Service service = MendeleyOAuthApi.service())
    {
      accessToken = service.getAccessToken(verificationCode);
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

    SyncBibDlgCtrlr.sync();

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

    String verificationCode = tfVerificationCode.getText();
    tfVerificationCode.clear();

    try (OAuth10aService service = ZoteroOAuthApi.service())
    {
      accessToken = service.getAccessToken(requestToken, verificationCode);
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

    SyncBibDlgCtrlr.sync();

    setUnlinkMessage();
    tabLinkToExtBibMgr.setContent(apUnlinkFromExtBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setLinkedMessage(String additionalInfo)
  {
    String message = "This database is currently linked to a " + db.getBibLibrary().type().getUserFriendlyName() + " library.";

    if (additionalInfo.length() > 0)
      message = message + ' ' + additionalInfo;

    lblCurrentlyLinked.setText(message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setUnlinkMessage()
  {
    LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> library = db.getBibLibrary();

    String userName = library.getUserName();

    if (userName.length() > 0)
      userName = "Username: " + userName;

    if ((userName.length() > 0) || (library.type() != LibraryType.ltMendeley))
    {
      setLinkedMessage(userName);
      return;
    }

    setLinkedMessage("Getting username from server...");

    ((MendeleyWrapper)library).getUserNameFromServer(
      emailAddress -> setLinkedMessage("Username: " + emailAddress),
      ex           -> setLinkedMessage("Unable to retrieve username from " + library.type().getUserFriendlyName() + " server."));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnUnlinkClick()
  {
    String typeName = db.getBibLibrary().type().getUserFriendlyName();

    if (confirmDialog("Do you really want to unlink your " + typeName + " account?" + System.lineSeparator() +
                      "All associations between " + appTitle + " records and " + typeName + " entries will be erased." + System.lineSeparator() +
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
    fileChooser.setInitialDirectory(new File(userWorkingDir()));

    nullSwitch(ui.windows.showOpenDialog(fileChooser, owner), filePath -> tf.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setExtFileTooltip()
  {
    tfExtFiles.setTooltip(new WebTooltip(

      "This setting should be used when you want multiple Hypernomicon databases to share PDF files<br>" +
      "so that you don't have to have multiple copies of the same file.<br><br>" +

      "Use the [...] button to select the path where the shared PDF files are located. It is a good idea<br>" +
      "to select the root folder of another database that will be the \"owner\" of the files.<br><br>" +

      "Once the external file path is set, you can drag and drop a file that lives under the external file<br>" +
      "path onto the URL field on the Works tab. Its URL will start with " + EXT_1 + ".<br><br>" +

      "Page numbers for that work can be set in the Preview window or the Contents window (accessed from<br>" +
      "the \"Show contents\" button in the Preview window).<br><br>" +

      "Warning: If the file is moved or renamed, e.g. using the File Manager while the \"owner\" database<br>" +
      "is open, the path will not be automatically updated in the corresponding work in this database."));

    setToolTip(btnExtFilesHelp, "Show help");

    btnExtFilesHelp.setOnMouseClicked(event ->
    {
      tfExtFiles.getTooltip().show(tfExtFiles, event.getScreenX() + 7, event.getScreenY() + 10);

      btnExtFilesHelp.setOnMouseExited(exitEvent ->
      {
        tfExtFiles.getTooltip().hide();
        btnExtFilesHelp.setOnMouseExited(null);
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
