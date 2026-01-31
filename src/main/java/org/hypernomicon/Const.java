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

package org.hypernomicon;

import java.util.Map;

import org.hypernomicon.util.VersionNumber;

import javafx.scene.input.DataFormat;

//---------------------------------------------------------------------------

public final class Const
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Const() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  /**
   * Change this and the version in pom.xml to set the application version
   */
  public static final VersionNumber appVersion = new VersionNumber(1, 33);  // 1.33

  /**
   * This is the minimum version that the application version (or higher) is able to load
   */
  public static final Map<VersionNumber, VersionNumber> appVersionToMinRecordsXMLVersion = Map.of(new VersionNumber(1), new VersionNumber(1));

  /**
   * This is the minimum version that the application version (or higher) is able to load
   */
  public static final Map<VersionNumber, VersionNumber> appVersionToMinSettingsXMLVersion = Map.of(new VersionNumber(1), new VersionNumber(1));

  /**
   * This is the version that the application version (or lower) will actually save to
   */
  public static final Map<VersionNumber, VersionNumber> appVersionToMaxRecordsXMLVersion = Map.ofEntries
  (
    Map.entry(new VersionNumber(1, 17, 5), new VersionNumber(1    )),
    Map.entry(new VersionNumber(1, 19, 4), new VersionNumber(1, 1 )),
    Map.entry(new VersionNumber(1, 20   ), new VersionNumber(1, 2 )),
    Map.entry(new VersionNumber(1, 22, 1), new VersionNumber(1, 3 )),
    Map.entry(new VersionNumber(1, 22, 2), new VersionNumber(1, 4 )),
    Map.entry(new VersionNumber(1, 23, 1), new VersionNumber(1, 5 )),
    Map.entry(new VersionNumber(1, 25, 1), new VersionNumber(1, 6 )),
    Map.entry(new VersionNumber(1, 28   ), new VersionNumber(1, 7 )),
    Map.entry(new VersionNumber(1, 29   ), new VersionNumber(1, 8 )),
    Map.entry(new VersionNumber(1, 31, 1), new VersionNumber(1, 9 )),
    Map.entry(new VersionNumber(1, 32, 1), new VersionNumber(1, 10)),
    Map.entry(appVersion                 , new VersionNumber(1, 11))
  );

  /**
   * This is the version that the application version (or lower) will actually save to
   */
  public static final Map<VersionNumber, VersionNumber> appVersionToMaxSettingsXMLVersion = Map.ofEntries
  (
    Map.entry(new VersionNumber(1, 17, 5), new VersionNumber(1   )),
    Map.entry(new VersionNumber(1, 19, 4), new VersionNumber(1, 1)),
    Map.entry(new VersionNumber(1, 22, 1), new VersionNumber(1, 2)),
    Map.entry(new VersionNumber(1, 23, 1), new VersionNumber(1, 3)),
    Map.entry(new VersionNumber(1, 30, 3), new VersionNumber(1, 4)),
    Map.entry(appVersion                 , new VersionNumber(1, 5))
  );

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final DataFormat HYPERNOMICON_DATA_FORMAT = new DataFormat("application/Hypernomicon");

  public static final double

    DEFAULT_FONT_SIZE = 11.0;

  public static final int

    INITIAL_NAV_LIST_SIZE = 150,
    BUTTON_MENU_DELAY_MS = 300,
    IGNORE_ARROW_KEYS_IN_TAB_PANE_MS = 300,
    FILENAME_LENGTH_TO_SHOW_TOOLTIP = 30,
    MAX_TOOLTIP_WIDTH = 1100,
    HDB_MRU_SIZE = 4;

  public static final String

    HDB_DEFAULT_FILENAME = "database.hdb",
    BLANK_DB_RESOURCE_NAME = "resources/blank_db.zip",
    EXT_1 = "ext1://";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class PrefKey
  {
    public static final String

      SETTINGS_VERSION = "settingsVersion",
      NOTIFY_USER_NOT_LINKED = "notifyUnlinked",
      DB_CREATION_DATE = "dbCreationDate",
      SOURCE_PATH = "sourcePath",
      SOURCE_FILENAME = "sourceFile",
      TRANSIENT_TEST_PARENT_PATH = "transientTestParentPath",
      TRANSIENT_TEST_FOLDER_NAME = "transientTestFolderName",
      LINK_GENERATION_LOG_FOLDER = "linkGenLogFolderPath",
      LOG_PATH = "logPath",
      HDB_MRU = "hbdMru",
      AUTO_IMPORT = "autoImport",
      IMAGE_EDITOR = "imageEditor",
      EXT_FILES_1 = "extFiles1",
      OFFICE_PATH = "officePath",
      IMAGE_EDITOR_COMMANDS = "imageEditorCommands",
      IMAGE_EDITOR_COMMAND_TYPE = "imageEditorCommandType",
      PDF_READER = "pdfReader",
      PDF_READER_COMMANDS = "pdfReaderCommands",
      PDF_READER_COMMAND_TYPE = "pdfReaderCommandType",
      THESIS_FOLDER_IS_BOOKS = "thesisFolderIsBooks",

      RECORD_TYPE = "typeID",
      DISPLAY_RECORD_TYPE = "displayTypeID",
      DEFAULT_CHAPTER_WORK_TYPE_ID = "defaultChapterWorkTypeID",
      SENTENCE_CASE = "sentenceCase",
      CHECK_INTERNET = "checkInternet",
      CHECK_FOR_NEW_VERSION = "checkForNewVersion",
      AUTO_OPEN_PDF = "autoOpenWorkFile",
      AUTO_RETRIEVE_BIB = "autoRetrieveBib",
      IMPORT_ACTION_DEFAULT = "importActionDefault",
      LINUX_WORKAROUND = "linuxWindowResizabilityWorkaround",
      PDFJS_SIDEBAR_VIEW = "pdfjsSidebarView",
      FONT_SIZE = "fontSize",
      DEF_DESC_FONT_SIZE = "defDescFontSize",
      DEF_DESC_FONT_FAMILY = "defDescFontFamily",
      DONT_OPEN_EMPTY_KEY_WORKS = "openEmptyKeyWorks",
      TERM_REQUIRE_SEARCH_KEY = "termRequireSearchKey",

      LOWER_CASE_TARGET_NAMES = "lowerCaseTargetNames",
      ARG_NAMING_MULTIPLE_AUTHORS = "multipleAuthorsInArgNames",
      ARG_NAMING_OXFORD_COMMA = "oxfordCommaInArgNames",
      ARG_TRUNCATE_NUM = "authorNumToTruncateInArgNames",
      ARG_AUTHORS_TO_SHOW = "authorsToShowWhenTruncatingInArgNames",
      ARG_TRUNCATION_INDICATOR = "truncationIndicatorInArgNames",
      ARG_FINAL_CONJ_SYMBOL = "finalConjSymbolInArgNames",

      WINDOW_X = "windowPositionX",
      WINDOW_Y = "windowPositionY",
      WINDOW_WIDTH = "windowWidth",
      WINDOW_HEIGHT = "windowHeight",
      WINDOW_ICONIFIED = "windowIconified",
      WINDOW_FULLSCREEN = "windowFullScreen",
      WINDOW_MAXIMIZED = "windowMaximized",

      PREV_WINDOW_X = "prevWindowPositionX",
      PREV_WINDOW_Y = "prevWindowPositionY",
      PREV_WINDOW_WIDTH = "prevWindowWidth",
      PREV_WINDOW_HEIGHT = "prevWindowHeight",

      CONTENTS_WINDOW_X = "contentsWindowPositionX",
      CONTENTS_WINDOW_Y = "contentsWindowPositionY",
      CONTENTS_WINDOW_WIDTH = "contentsWindowWidth",
      CONTENTS_WINDOW_HEIGHT = "contentsWindowHeight",

      BM_WINDOW_X = "bmWindowPositionX",
      BM_WINDOW_Y = "bmWindowPositionY",
      BM_WINDOW_WIDTH = "bmWindowWidth",
      BM_WINDOW_HEIGHT = "bmWindowHeight",

      FM_WINDOW_X = "fmWindowPositionX",
      FM_WINDOW_Y = "fmWindowPositionY",
      FM_WINDOW_WIDTH = "fmWindowWidth",
      FM_WINDOW_HEIGHT = "fmWindowHeight",

      BIB_LIBRARY_VERSION = "bibLibraryVersion",
      BIB_LAST_SYNC_TIME = "bibLastSyncTime",
      BIB_API_KEY = "bibApiKey",
      BIB_USER_ID = "bibUserID",
      BIB_USER_NAME = "bibUserName",
      BIB_UNIT_TEST_USER_ID = "bibUnitTestUserID",
      BIB_ACCESS_TOKEN = "bibAccessToken",
      BIB_REFRESH_TOKEN = "bibRefreshToken",
      BIB_LIBRARY_TYPE = "bibType",
      BIB_SRCH_REQUIRE_BY_DEFAULT = "bibRequireByDefault",

      KEY_WORK_SORT_BY_NAME = "keyWorkSortByName",
      RIGHT_CLICK_TO_LAUNCH = "rtClkToLaunch",

      WEB_BUTTONS = "webButtons",

      XML_DIFF_BACKUP_PATH = "xmlDiffBackupPath",
      XML_DIFF_TEST_PATH   = "xmlDiffTestPath",
      XML_DIFF_BACKUP_TEMP_PATH = "xmlDiffBackupTempPath",
      XML_DIFF_TEST_TEMP_PATH = "xmlDiffTestTempPath",

      XML_DIFF_EXECUTABLE_1 = "xmlDiffExecutable1",
      XML_DIFF_EXECUTABLE_2 = "xmlDiffExecutable2",
      XML_DIFF_EXECUTABLE_3 = "xmlDiffExecutable3",
      XML_DIFF_EXECUTABLE_4 = "xmlDiffExecutable4",

      XML_DIFF_SELECTED_EXE_NUM = "xmlDiffSelectedExeNum";

    private PrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class FilenameRulesPrefKey
  {
    public static final String

      CASE_INSENSITIVE         = "filenameRulesCaseInsensitive",
      UNICODE_COMP_INSENSITIVE = "filenameRulesUnicodeCompInsensitive",
      TRIMS_TRAILING           = "filenameRulesTrimsTrailing",
      IGNORES_IGNORABLES       = "filenameRulesIgnoresIgnorables",
      CASE_FOLDING_MODE        = "filenameRulesCaseFoldingMode",
      INITIALIZED              = "filenameRulesInitialized";

    private FilenameRulesPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class ZoomPrefKey
  {
    public static final String

      MAINTEXT = "mainTextZoom",
      BIBMGR   = "bibMgrZoom",
      FILEMGR  = "fileMgrZoom",
      QUERYTAB = "queryTabZoom",
      TREETAB  = "treeTabZoom";

    private ZoomPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class WebButtonContextPrefKey
  {
    public static final String

      PERSON     = "personSrch",
      WORK       = "workSrch",
      GEN        = "genSrch",
      PERSON_IMG = "personImgSrch",
      INST       = "instSrch",
      INST_MAP   = "instMapSrch",
      DOI        = "doiSrch",
      ISBN       = "isbnSrch";

    private WebButtonContextPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class RecordIDPrefKey
  {
    public static final String

      PERSON        = "personID",
      DEBATE        = "debateID",
      INSTITUTION   = "institutionID",
      INVESTIGATION = "investigationID",
      POSITION      = "positionID",
      ARGUMENT      = "argumentID",
      CONCEPT       = "conceptID",
      WORK          = "workID",
      FILE          = "fileID",
      NOTE          = "noteID";

    private RecordIDPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class FolderIDPrefKey
  {
    public static final String

      XML        = "xmlFolderID",
      PICTURES   = "picturesFolderID",
      BOOKS      = "booksFolderID",
      PAPERS     = "papersFolderID",
      RESULTS    = "resultsFolderID",
      UNENTERED  = "unenteredFolderID",
      MISC_FILES = "miscFilesFolderID",
      TOPICAL    = "topicalFolderID";

    private FolderIDPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class FileNamePrefKey
  {
    public static final String

      COMPONENT_COUNT = "fnComponentCount",
      COMPONENT = "fnComponent",
      EXCL_WORK_TYPES = "fnExclWorkTypes",
      WITHIN_SEP = "fnWithinSep",
      BEFORE_SEP = "fnBeforeSep",
      AFTER_SEP = "fnAfterSep",
      TEST = "fnTest",

      TREAT_ED_AS_AUTHOR = "fnTreatEdAsAuthor",
      ADD_INITIAL = "fnAddInitial",
      YEAR_LETTER = "fnYearLetter",
      POSIX = "fnPosix",
      LOWERCASE = "fnLowercase",
      MAX_CHAR = "fnMaxChar";

    private FileNamePrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class DividerPositionPrefKey
  {
    public static final String

      PERSON_TOP_HORIZ    = "personTopHoriz",
      PERSON_MID_VERT     = "personMidVert",
      PERSON_BOTTOM_VERT  = "personBottomVert",
      INST_MID_HORIZ      = "instMidHoriz",
      WORK_MID_VERT       = "workMidVert",
      WORK_BOTTOM_VERT    = "workBottomVert",
      WORK_RIGHT_HORIZ    = "workRightHoriz",
      WORK_BOTTOM_HORIZ   = "workBottomHoriz",
      FILE_BOTTOM_VERT    = "fileBottomVert",
      FILE_RIGHT_HORIZ    = "fileRightHoriz",
      FILE_RIGHT_VERT     = "fileRightVert",
      DEBATE_TOP_VERT     = "debateTopVert",
      DEBATE_BOTTOM_VERT  = "debateBottomVert",
      DEBATE_BOTTOM_HORIZ = "debateBottomHoriz",
      POS_TOP_VERT        = "posTopVert",
      POS_BOTTOM_VERT     = "posBottomVert",
      POS_BOTTOM_HORIZ    = "posBottomHoriz",
      ARG_TOP_VERT        = "argTopVert",
      ARG_BOTTOM_VERT     = "argBottomVert",
      NOTE_TOP_VERT       = "noteTopVert",
      NOTE_BOTTOM_VERT    = "noteBottomVert",
      NOTE_BOTTOM_HORIZ   = "noteBottomHoriz",
      TERM_TOP_VERT       = "termTopVert",
      TERM_BOTTOM_VERT    = "termBottomVert",
      TERM_BOTTOM_HORIZ   = "termBottomHoriz",
      MGR_MAIN_HORIZ      = "mgrMainHoriz",
      MGR_FILES_VERT      = "mgrFilesVert",
      MGR_RECORDS_HORIZ   = "mgrRecordsHoriz",
      BIB_LEFT_HORIZ      = "bibLeftHoriz",
      BIB_RIGHT_HORIZ     = "bibRightHoriz";

    private DividerPositionPrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class TablePrefKey
  {
    public static final String

      ARG_PARENTS     = "htArgParents",
      ARG_SRC         = "htArgSrc",
      ARG_RESPONSES   = "htArgCounters",
      CONTENTS_DLG    = "htContentsDlg",
      DEBATE_PARENTS  = "htDebateParents",
      DEBATE_POS      = "htDebatePos",
      DEBATE_SUB      = "htDebateSub",
      FM_RECORDS      = "htFmRecords",
      FILE_AUTHORS    = "htFileAuthors",
      FILE_LABELS     = "htFileLabels",
      FILE_MENTIONERS = "htFileMentioners",
      INST_SUB        = "htInstSub",
      INST_PEOPLE     = "htInstPeople",
      FIND            = "htFind",
      NOTE_PARENTS    = "htNoteParents",
      NOTE_SUB        = "htNoteSub",
      NOTE_MENTIONERS = "htNoteMentioners",
      PERSON_INST     = "htPersonInst",
      PERSON_WORKS    = "htPersonWorks",
      PERSON_ARG      = "htPersonArg",
      POS_PARENTS     = "htPosParents",
      POS_ARG         = "htPosArg",
      POS_SUB         = "htPosSub",
      QUERY_FIELDS    = "htQueryFields",
      TERM_GLOSSARIES = "htTermGlossaries",
      CONCEPT_SUB     = "htSubConcepts",
      TERM_DISPLAYERS = "htTermDisplayers",
      TREE            = "htTree",
      WORK_AUTHORS    = "htWorkAuthors",
      WORK_DLG        = "htWorkDlg",
      WORK_LABELS     = "htWorkLabels",
      WORK_SUB        = "htWorkSub",
      WORK_MENTIONERS = "htWorkMentioners",
      WORK_ARG        = "htWorkArg",
      WORK_FILES      = "htWorkFiles",
      WORK_MISC       = "htWorkMisc",
      MGR_FILES       = "htMgrFiles",
      BIB_ENTRIES     = "htBibEntries";

    private TablePrefKey() { throw new UnsupportedOperationException("Instantiation is not allowed."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
