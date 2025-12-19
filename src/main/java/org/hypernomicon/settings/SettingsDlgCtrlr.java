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

package org.hypernomicon.settings;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

import com.github.scribejava.core.exceptions.OAuthException;
import com.github.scribejava.core.model.*;
import com.github.scribejava.core.oauth.OAuth10aService;
import com.github.scribejava.core.oauth.OAuth20Service;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import org.hypernomicon.App;
import org.hypernomicon.bib.BibCollection;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.mendeley.*;
import org.hypernomicon.bib.mendeley.auth.MendeleyAuthKeys;
import org.hypernomicon.bib.zotero.*;
import org.hypernomicon.bib.zotero.auth.ZoteroAuthKeys;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.VBox;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.settings.SettingsDlgCtrlr.SettingsPage.*;

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

//---------------------------------------------------------------------------

public class SettingsDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apLinkToExtBibMgr, apUnlinkFromExtBibMgr;
  @FXML private Button btnCodePaste, btnUnlink, btnVerify, btnImgEditorAdvanced, btnPdfViewerAdvanced, btnExtFilesHelp, btnReEstablishAccess;
  @FXML private CheckBox chkAutoOpenPDF, chkNewVersionCheck, chkAutoRetrieveBib, chkInternet, chkUseSentenceCase, chkDefaultChapterWorkType, chkLinuxWorkaround,
                         chkCompDontExpandKeyWorks, chkDBDontExpandKeyWorks, chkTermRequireSearchKey;
  @FXML private ComboBox<HyperTableCell> cbDefaultChapterWorkType;
  @FXML private Label lblCurrentlyLinked, lblRedirect, lblStep2, lblStep2Instructions,
                      lblStep3, lblStep3Instructions, lblStep4, lblStep4Instructions;
  @FXML private Slider sliderFontSize;
  @FXML private Tab tabLinkToExtBibMgr, tabComputerSpecific, tabDBSpecific, tabFolders, tabNaming, tabWorkSearchKey, tabArgNaming, tabWebButtons, tabShortcuts;
  @FXML private TabPane tpMain;
  @FXML private TextField tfImageEditor, tfPDFReader, tfExtFiles, tfLogPath, tfOffice, tfVerificationCode;
  @FXML private ToggleButton btnZoteroAuthorize, btnMendeleyAuthorize;
  @FXML private VBox vbRefMgr;

  @FXML TreeView<SettingsPage> treeView;

  private final HyperCB hcbDefaultChapterWorkType;
  private final StringProperty authUrl;
  private final boolean noDB;
  private final Set<SettingsControl> settingsCtrlrs;
  private final Map<SettingsPage, Tab> pageToTab = new EnumMap<>(SettingsPage.class);
  private final Map<SettingsPage, TreeItem<SettingsPage>> pageToTreeItem = new EnumMap<>(SettingsPage.class);

  private OAuth1RequestToken requestToken;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum SettingsPage
  {
    CompGeneral("Settings applying to this computer"),
    WebSearch("Web Search Buttons"),
    Shortcuts("User-Defined Keyboard Shortcuts"),
    DBSpecific("Settings applying to this database"),
    Folders("Reassign Folders"),
    WorkNaming("Work File Naming"),
    WorkSearchKey("Work Search Key Generation"),
    ArgNaming("Argument/Stance Naming"),
    BibMgr("Bibliography Manager");

    private final String caption;

    SettingsPage(String caption) { this.caption = caption; }
    @Override public String toString()   { return caption; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

  @FXML private void btnImageEditorBrowseClick() { browseClick(tfImageEditor); }
  @FXML private void btnPDFReaderClick()         { browseClick(tfPDFReader); }
  @FXML private void btnClearExtPathClick()      { tfExtFiles.clear(); }
  @FXML private void mnuClearLogPathClick()      { tfLogPath.clear(); }
  @FXML private void btnClearOfficeClick()       { tfOffice.clear(); }

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

    noDB = (db.prefs == null) || db.isOffline();

    initTree();

    setExtFileTooltip();

    settingsCtrlrs = Set.of
    (
      initControl(tabWebButtons   , "WebButtonSettings"     ),
      initControl(tabShortcuts    , "shortcuts/Shortcuts"   ),
      initControl(tabFolders      , "FolderSettings"        ),
      initControl(tabNaming       , "WorkFileNamingSettings"),
      initControl(tabWorkSearchKey, "WorkSearchKeys"        ),
      initControl(tabArgNaming    , "ArgumentNaming"        )
    );

    btnVerify.setOnAction(event ->
    {
      if (tfVerificationCode.textProperty().isEmpty().get())
      {
        falseWithWarningPopup("You must enter a verification code.", tfVerificationCode);
        return;
      }

      if (btnZoteroAuthorize.isSelected())
        zoteroVerifyClick();
      else
        mendeleyVerifyClick();
    });

    btnUnlink.setOnAction(event -> btnUnlinkClick());

    btnReEstablishAccess.setOnAction(event ->
    {
      btnReEstablishAccess.setDisable(true);
      vbRefMgr.getChildren().add(apLinkToExtBibMgr);
      selectLibraryType(db.getBibLibrary().type());
    });

    setToolTip(btnReEstablishAccess, "Renew access to reference manager account. Use this option only if there is a problem with syncing.");

    authUrl = new SimpleStringProperty();

    btnZoteroAuthorize  .disableProperty().bind(authUrl.isNotEmpty());
    btnMendeleyAuthorize.disableProperty().bind(authUrl.isNotEmpty());
    lblRedirect.visibleProperty().bind(authUrl.isNotEmpty());

    btnZoteroAuthorize  .setOnAction(event -> btnAuthorizeClick(LibraryType.ltZotero));
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

    initTextField(app.prefs, tfImageEditor, PrefKey.IMAGE_EDITOR, "");
    initTextField(app.prefs, tfPDFReader  , PrefKey.PDF_READER  , "");
    initTextField(app.prefs, tfLogPath    , PrefKey.LOG_PATH    , "");
    initTextField(app.prefs, tfExtFiles   , PrefKey.EXT_FILES_1 , "");
    initTextField(app.prefs, tfOffice     , PrefKey.OFFICE_PATH , "");

    btnImgEditorAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = new LaunchCommandsDlgCtrlr
        ("Modify Image Editor Command(s)", PrefKey.IMAGE_EDITOR, PrefKey.IMAGE_EDITOR_COMMANDS, PrefKey.IMAGE_EDITOR_COMMAND_TYPE);

      if (lcdc.showModal())
        tfImageEditor.setText(app.prefs.get(PrefKey.IMAGE_EDITOR, ""));
    });

    btnPdfViewerAdvanced.setOnAction(event ->
    {
      LaunchCommandsDlgCtrlr lcdc = new LaunchCommandsDlgCtrlr
        ("Modify PDF Viewer Command(s)", PrefKey.PDF_READER, PrefKey.PDF_READER_COMMANDS, PrefKey.PDF_READER_COMMAND_TYPE);

      if (lcdc.showModal())
        tfPDFReader.setText(app.prefs.get(PrefKey.PDF_READER, ""));
    });

    setToolTip(btnImgEditorAdvanced, "Open dialog to customize how the image editor is launched");
    setToolTip(btnPdfViewerAdvanced, "Open dialog to customize how the PDF viewer is launched");

    sliderFontSize.setValue(app.prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE));
    sliderFontSize.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((oldValue == null) || (newValue == null) || (oldValue.doubleValue() == newValue.doubleValue())) return;
      if (app.prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE) == newValue.doubleValue()) return;

      app.prefs.putDouble(PrefKey.FONT_SIZE, newValue.doubleValue());
    });

    setToolTip(sliderFontSize, "Base font size");

    initCheckBox(app.prefs, chkInternet              , PrefKey.CHECK_INTERNET           , true );
    initCheckBox(app.prefs, chkNewVersionCheck       , PrefKey.CHECK_FOR_NEW_VERSION    , true );
    initCheckBox(app.prefs, chkAutoOpenPDF           , PrefKey.AUTO_OPEN_PDF            , true );
    initCheckBox(app.prefs, chkAutoRetrieveBib       , PrefKey.AUTO_RETRIEVE_BIB        , true );
    initCheckBox(app.prefs, chkCompDontExpandKeyWorks, PrefKey.DONT_OPEN_EMPTY_KEY_WORKS, false);

    chkLinuxWorkaround.setVisible(IS_OS_LINUX);

    initCheckBox(app.prefs, chkLinuxWorkaround, PrefKey.LINUX_WORKAROUND, false);

    disableAllIff(noDB, tabDBSpecific, tabFolders, tabNaming, tabArgNaming);

    hcbDefaultChapterWorkType = new HyperCB(cbDefaultChapterWorkType, ctEditableLimitedDropDown, new StandardPopulator(hdtWorkType));

    cbDefaultChapterWorkType.disableProperty().bind(chkDefaultChapterWorkType.selectedProperty().not());

    if (noDB == false)
    {
      initCheckBox(db.prefs, chkUseSentenceCase     , PrefKey.SENTENCE_CASE            , false);
      initCheckBox(db.prefs, chkDBDontExpandKeyWorks, PrefKey.DONT_OPEN_EMPTY_KEY_WORKS, false);
      initCheckBox(db.prefs, chkTermRequireSearchKey, PrefKey.TERM_REQUIRE_SEARCH_KEY  , true );
      initDefaultChapterWorkType();
    }

    stage.setOnHiding(event ->
    {
      setLogPath(new FilePath(tfLogPath.getText()));

      settingsCtrlrs.forEach(ctrlr -> ctrlr.save(noDB));
    });

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

    addTreeItem(CompGeneral  , tabComputerSpecific, null       );
    addTreeItem(DBSpecific   , tabDBSpecific      , null       );
    addTreeItem(WebSearch    , tabWebButtons      , CompGeneral);
    addTreeItem(Shortcuts    , tabShortcuts       , CompGeneral);
    addTreeItem(Folders      , tabFolders         , DBSpecific );
    addTreeItem(WorkNaming   , tabNaming          , DBSpecific );
    addTreeItem(WorkSearchKey, tabWorkSearchKey   , DBSpecific );
    addTreeItem(ArgNaming    , tabArgNaming       , DBSpecific );
    addTreeItem(BibMgr       , tabLinkToExtBibMgr , DBSpecific );

    root.getChildren().add(pageToTreeItem.get(CompGeneral));

    if (db.isOnline()) root.getChildren().add(pageToTreeItem.get(DBSpecific));

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        tpMain.getSelectionModel().select(pageToTab.get(newValue.getValue()));
    });

    if (db.isOnline() && db.bibLibraryIsLinked())
    {
      setUnlinkMessage();

      vbRefMgr.getChildren().remove(1);
    }
    else
    {
      vbRefMgr.getChildren().removeFirst();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public interface SettingsControl { void init(boolean noDB); void save(boolean noDB); }

  private SettingsControl initControl(Tab tab, String fxmlName)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("settings/" + fxmlName + ".fxml"));
      AnchorPane ap = loader.load();
      tab.setContent(ap);
      SettingsControl ctrlr = loader.getController();
      ctrlr.init(noDB);
      return ctrlr;
    }
    catch (IOException e)
    {
      logThrowable(e);
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void initTextField(Preferences prefs, TextField tf, String prefKey, String defValue)
  {
    initTextField(prefs, tf, prefKey, defValue, null);
  }

  static void initTextField(Preferences prefs, TextField tf, String prefKey, String defValue, Consumer<String> handler)
  {
    tf.setText(prefs.get(prefKey, defValue));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      prefs.put(prefKey, newValue);
      if (handler != null) handler.accept(newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void initCheckBox(Preferences prefs, CheckBox chk, String prefKey, boolean defValue)
  {
    initCheckBox(prefs, chk, prefKey, defValue, null);
  }

  static void initCheckBox(Preferences prefs, CheckBox chk, String prefKey, boolean defValue, Consumer<Boolean> handler)
  {
    chk.setSelected(prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      prefs.putBoolean(prefKey, newValue);
      if (handler != null) handler.accept(newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Initializes a group of {@link ToggleButton}s with persistent preferences and behavior management.
   * Ensures that the correct button is selected based on stored preferences,
   * and updates preferences dynamically when a button is toggled.<br>
   * Each ToggleButton is associated with a String value that will be saved to the preference key
   * when the button is selected.
   *
   * @param prefs          The {@link Preferences} object used to persist user settings.
   * @param prefKey        The key in the preferences to store the selected toggle button.
   * @param refreshHandler A {@link Runnable} that is executed whenever a toggle button is selected.
   *                       Can be null if no refresh action is required.
   * @param defValue       The default {@link ToggleButton} to select if no preference is set or an invalid preference exists.
   * @param map            A map linking preference keys to {@link ToggleButton} instances.
   *                       Keys represent the preference value associated with each button.
   */
  static void initToggleButtons(Preferences prefs, String prefKey, Runnable refreshHandler, ToggleButton defValue, Map<String, ToggleButton> map)
  {
    BiMap<String, ToggleButton> biMap = HashBiMap.create(map);

    String startStr = prefs.get(prefKey, "");
    ToggleButton startButton = startStr.isBlank() ? defValue : map.get(startStr);
    if (startButton == null) startButton = defValue;

    if (startButton != null)
      startButton.setSelected(true);
    else
      map.values().forEach(toggleButton -> toggleButton.setSelected(false));

    map.values().forEach(toggleButton -> toggleButton.setOnAction(event -> Platform.runLater(() ->
    {
      prefs.put(prefKey, nullSwitch(toggleButton.getToggleGroup().getSelectedToggle(), "", biMap.inverse()::get));

      if (refreshHandler != null) refreshHandler.run();
    })));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initDefaultChapterWorkType()
  {
    HDT_WorkType workType = db.workTypes.getByID(db.prefs.getInt(PrefKey.DEFAULT_CHAPTER_WORK_TYPE_ID, -1));

    chkDefaultChapterWorkType.setSelected(workType != null);

    hcbDefaultChapterWorkType.selectIDofRecord(workType);

    chkDefaultChapterWorkType.selectedProperty().addListener((ob, ov, nv) ->
    {
      if (nv == null) return;

      if (Boolean.FALSE.equals(nv))
      {
        db.prefs.putInt(PrefKey.DEFAULT_CHAPTER_WORK_TYPE_ID, -1);
        return;
      }

      db.prefs.putInt(PrefKey.DEFAULT_CHAPTER_WORK_TYPE_ID, hcbDefaultChapterWorkType.selectedID());
    });

    hcbDefaultChapterWorkType.addListener((ov, nv) ->
    {
      int workTypeID = HDT_Record.getIDSafe(HyperTableCell.getRecord(nv));
      db.prefs.putInt(PrefKey.DEFAULT_CHAPTER_WORK_TYPE_ID, workTypeID);
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
      if (db.isOnline())
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

    nullSwitch(showDirDialog(dirChooser), filePath -> tfExtFiles.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLogPathBrowseClick()
  {
    FilePath startPath = new FilePath(tfLogPath.getText()).getDirOnly();

    if (FilePath.isEmpty(startPath))
      startPath = new FilePath(userWorkingDir());

    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Log files (*.log)", "*.log"));
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(startPath.toFile());

    fileChooser.setTitle("Select Log File");

    nullSwitch(showSaveDialog(fileChooser), filePath -> tfLogPath.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnOfficeBrowseClick()
  {
    FilePath startPath = new FilePath(tfOffice.getText());

    if (FilePath.isEmpty(startPath))
      startPath = new FilePath(userWorkingDir());

    if (IS_OS_MAC)
    {
      FileChooser fileChooser = new FileChooser();

      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("App files (*.app)", "*.app"));
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
      fileChooser.setInitialDirectory(startPath.toFile());

      fileChooser.setTitle("Select Office Application");

      nullSwitch(showOpenDialog(fileChooser), filePath -> tfOffice.setText(filePath.toString()));
      return;
    }

    DirectoryChooser dirChooser = new DirectoryChooser();

    dirChooser.setInitialDirectory(startPath.toFile());
    dirChooser.setTitle("Select Folder");

    nullSwitch(showDirDialog(dirChooser), filePath -> tfOffice.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectLibraryType(LibraryType libraryType)
  {
    if (db.isOffline()) return;

    switch (libraryType)
    {
      case ltMendeley : btnMendeleyAuthorize.setSelected(true); break;
      case ltZotero   : btnZoteroAuthorize  .setSelected(true); break;
    }

    btnAuthorizeClick(libraryType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnAuthorizeClick(LibraryType libType)
  {
    try
    {
      lblStep2Instructions.setText("Click link to open web site where you will authorize " + appTitle + " to access your " +
                                   libType.userFriendlyName + " account and get verification code.");

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
    catch (IOException | InterruptedException | ExecutionException e)
    {
      errorPopup("Error: " + getThrowableMessage(e));

      btnZoteroAuthorize  .setSelected(false);
      btnMendeleyAuthorize.setSelected(false);

      if (db.bibLibraryIsLinked())
      {
        btnReEstablishAccess.setDisable(false);
        vbRefMgr.getChildren().setAll(apUnlinkFromExtBibMgr);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mendeleyVerifyClick()
  {
    boolean success = false;

    OAuth2AccessToken token = null;

    String verificationCode = tfVerificationCode.getText();

    try (OAuth20Service service = MendeleyOAuthApi.service())
    {
      token = service.getAccessToken(verificationCode);
      success = true;
    }
    catch (OAuthException e)                                          { errorPopup("Verification code was rejected by the server."); }
    catch (IOException | InterruptedException | ExecutionException e) { errorPopup("Verification failed: " + getThrowableMessage(e)); }

    if (success == false) return;

    MendeleyAuthKeys authKeys = MendeleyAuthKeys.createFromOauthToken(token);

    try
    {
      MendeleyWrapper dummyWrapper = MendeleyWrapper.getProfileInfoFromServer(authKeys);

      if (db.bibLibraryIsLinked())
        db.getBibLibrary().enableSyncOnThisComputer(authKeys, dummyWrapper.getUserID(), dummyWrapper.getUserName(), true);
      else
        db.linkBibLibrary(LibraryType.ltMendeley, authKeys, dummyWrapper.getUserID(), dummyWrapper.getUserName());
    }
    catch (Exception e)
    {
      errorPopup("Verification failed: " + getThrowableMessage(e));
      return;
    }

    authUrl.set("");
    btnZoteroAuthorize  .setSelected(false);
    btnMendeleyAuthorize.setSelected(false);
    tfVerificationCode.clear();

    BibManager.instance().syncWithModalPopup();

    vbRefMgr.getChildren().setAll(apUnlinkFromExtBibMgr);
    btnReEstablishAccess.setDisable(false);
    setUnlinkMessage();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void zoteroVerifyClick()
  {
    boolean success = false;

    OAuth1AccessToken token = null;

    String verificationCode = tfVerificationCode.getText();

    try (OAuth10aService service = ZoteroOAuthApi.service())
    {
      token = service.getAccessToken(requestToken, verificationCode);
      success = true;
    }
    catch (OAuthException e)                                          { errorPopup("Verification code was rejected by the server."); }
    catch (IOException | InterruptedException | ExecutionException e) { errorPopup("Verification failed: " + getThrowableMessage(e)); }

    if (success == false) return;

    ZoteroAuthKeys authKeys = ZoteroAuthKeys.createFromOauthToken(token);

    try
    {
      if (db.bibLibraryIsLinked())
        db.getBibLibrary().enableSyncOnThisComputer(authKeys, token.getParameter("userID"), token.getParameter("username"), true);
      else
        db.linkBibLibrary(LibraryType.ltZotero, authKeys, token.getParameter("userID"), token.getParameter("username"));
    }
    catch (Exception e)
    {
      errorPopup("Verification failed: " + getThrowableMessage(e));
      return;
    }

    authUrl.set("");
    btnZoteroAuthorize  .setSelected(false);
    btnMendeleyAuthorize.setSelected(false);
    tfVerificationCode.clear();

    BibManager.instance().syncWithModalPopup();

    vbRefMgr.getChildren().setAll(apUnlinkFromExtBibMgr);
    btnReEstablishAccess.setDisable(false);
    setUnlinkMessage();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setLinkedMessage(String additionalInfo)
  {
    String message = "This database is currently linked to a " + db.bibLibraryUserFriendlyName() + " library.";

    if (strNotNullOrBlank(additionalInfo))
      message = message + ' ' + additionalInfo;

    lblCurrentlyLinked.setText(message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setUnlinkMessage()
  {
    LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> library = db.getBibLibrary();

    String userName = safeStr(library.getUserName());

    setLinkedMessage(userName.isBlank() ?
      "Getting username from server..."
    :
      "Username: " + userName);

    library.getProfileInfoFromServer(
      _userName -> setLinkedMessage("Username: " + _userName),
      ex        ->
      {
        if (userName.isBlank())
          setLinkedMessage("Unable to retrieve username from " + library.getUserFriendlyName() + " server.");
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnUnlinkClick()
  {
    String typeName = db.bibLibraryUserFriendlyName();

    if (confirmDialog("Do you really want to unlink your " + typeName + " account?" + System.lineSeparator() +
                      "All associations between " + appTitle + " records and " + typeName + " entries will be erased." + System.lineSeparator() +
                      "This process cannot be undone.", false) == false)
      return;

    db.unlinkBibLibrary();
    vbRefMgr.getChildren().setAll(apLinkToExtBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void browseClick(TextField tf)
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    fileChooser.setInitialDirectory(new File(userWorkingDir()));

    nullSwitch(showOpenDialog(fileChooser), filePath -> tf.setText(filePath.toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setExtFileTooltip()
  {
    tfExtFiles.setTooltip(new WebTooltip("""
      This setting should be used when you want multiple Hypernomicon databases to share PDF files<br>
      so that you don't have to have multiple copies of the same file.<br><br>

      Use the [...] button to select the path where the shared PDF files are located. It is a good idea<br>
      to select the root folder of another database that will be the &ldquo;owner&rdquo; of the files.<br><br>

      Once the external file path is set, you can drag and drop a file that lives under the external file<br>
      path onto the URL field on the Works tab. Its URL will start with %s.<br><br>

      Page numbers for that work can be set in the Preview window or the Contents window (accessed from<br>
      the &ldquo;Show contents&rdquo; button in the Preview window).<br><br>

      Warning: If the file is moved or renamed, e.g. using the File Manager while the &ldquo;owner&rdquo; database<br>
      is open, the path will not be automatically updated in the corresponding work in this database.""".formatted(EXT_1)));

    WebTooltip.setupClickHandler(btnExtFilesHelp, tfExtFiles);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuLogLaunchClick()
  {
    FilePath logFilePath = new FilePath(tfLogPath.getText());
    if (FilePath.isEmpty(logFilePath)) return;

    launchFile(logFilePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuLogShowInSysExplorerClick()
  {
    FilePath logFilePath = new FilePath(tfLogPath.getText());
    if (FilePath.isEmpty(logFilePath)) return;

    highlightFileInExplorer(logFilePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuLogCopyToClipboardClick()
  {
    FilePath logFilePath = new FilePath(tfLogPath.getText());
    if (FilePath.isEmpty(logFilePath)) return;

    List<String> s;

    try { s = logFilePath.readToStrList(); }
    catch (IOException e)
    {
      errorPopup("An error occurred while trying to read the log file: " + getThrowableMessage(e));
      return;
    }

    copyToClipboard(strListToStr(s, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
