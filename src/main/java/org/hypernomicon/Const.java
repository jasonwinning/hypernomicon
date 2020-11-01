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

package org.hypernomicon;

import java.util.Map;

import org.hypernomicon.util.VersionNumber;

import com.google.common.collect.ImmutableMap;

import javafx.scene.input.DataFormat;

public final class Const
{
  public static final DataFormat HYPERNOMICON_DATA_FORMAT = new DataFormat("application/Hypernomicon");

  public static final double

    DEFAULT_FONT_SIZE = 11.0;

  public static final int

    INITIAL_NAV_LIST_SIZE = 150,
    BUTTON_MENU_DELAY_MS = 300,
    IGNORE_ARROW_KEYS_IN_TAB_PANE_MS = 300,
    HDB_MRU_SIZE = 4,

    BLANK_FN_COMPONENT = 0,
    AUTHOR_FN_COMPONENT = 1,
    TITLE_FN_COMPONENT = 2,
    YEAR_FN_COMPONENT = 3,
    TRANS_FN_COMPONENT = 4,
    EDITOR_FN_COMPONENT = 5;

  public static final String

    PREF_KEY_SETTINGS_VERSION = "settingsVersion",
    PREF_KEY_NOTIFY_USER_NOT_LINKED = "notifyUnlinked",
    PREF_KEY_DB_CREATION_DATE = "dbCreationDate",
    PREF_KEY_SOURCE_PATH = "sourcePath",
    PREF_KEY_SOURCE_FILENAME = "sourceFile",
    HDB_DEFAULT_FILENAME = "database.hdb",
    PREF_KEY_HDB_MRU = "hbdMru",
    PREF_KEY_AUTO_IMPORT = "autoImport",
    PREF_KEY_IMAGE_EDITOR = "imageEditor",
    PREF_KEY_IMAGE_EDITOR_COMMANDS = "imageEditorCommands",
    PREF_KEY_IMAGE_EDITOR_COMMAND_TYPE = "imageEditorCommandType",
    PREF_KEY_PDF_READER = "pdfReader",
    PREF_KEY_PDF_READER_COMMANDS = "pdfReaderCommands",
    PREF_KEY_PDF_READER_COMMAND_TYPE = "pdfReaderCommandType",
    PREF_KEY_DO_INTERNET_CHECK = "doInternetCheck",

    PREF_KEY_XML_FOLDER_ID = "xmlFolderID",
    PREF_KEY_PICTURES_FOLDER_ID = "picturesFolderID",
    PREF_KEY_BOOKS_FOLDER_ID = "booksFolderID",
    PREF_KEY_PAPERS_FOLDER_ID = "papersFolderID",
    PREF_KEY_RESULTS_FOLDER_ID = "resultsFolderID",
    PREF_KEY_UNENTERED_FOLDER_ID = "unenteredFolderID",
    PREF_KEY_MISC_FILES_FOLDER_ID = "miscFilesFolderID",
    PREF_KEY_TOPICAL_FOLDER_ID = "topicalFolderID",

    PREF_KEY_RECORD_TYPE = "typeID",
    PREF_KEY_DISPLAY_RECORD_TYPE = "displayTypeID",
    PREF_KEY_PERSON_ID = "personID",
    PREF_KEY_DEBATE_ID = "debateID",
    PREF_KEY_INSTITUTION_ID = "institutionID",
    PREF_KEY_INVESTIGATION_ID = "investigationID",
    PREF_KEY_POSITION_ID = "positionID",
    PREF_KEY_ARGUMENT_ID = "argumentID",
    PREF_KEY_TERM_ID = "termID",
    PREF_KEY_WORK_ID = "workID",
    PREF_KEY_FILE_ID = "fileID",
    PREF_KEY_NOTE_ID = "noteID",
    PREF_KEY_SENTENCE_CASE = "sentenceCase",
    PREF_KEY_CHECK_INTERNET = "checkInternet",
    PREF_KEY_CHECK_FOR_NEW_VERSION = "checkForNewVersion",
    PREF_KEY_AUTO_OPEN_PDF = "autoOpenWorkFile",
    PREF_KEY_AUTO_RETRIEVE_BIB = "autoRetrieveBib",
    PREF_KEY_IMPORT_ACTION_DEFAULT = "importActionDefault",
    PREF_KEY_LINUX_WORKAROUND = "linuxWindowResizabilityWorkaround",
    PREF_KEY_PDFJS_SIDEBAR_VIEW = "pdfjsSidebarView",
    PREF_KEY_FONT_SIZE = "fontSize",

    PREF_KEY_IMPORT_ACTION_MOVE = "move",
    PREF_KEY_IMPORT_ACTION_COPY = "copy",
    PREF_KEY_IMPORT_ACTION_NONE = "none",

    PREF_KEY_HT_ARG_PARENTS = "htArgParents",
    PREF_KEY_HT_ARG_SRC = "htArgSrc",
    PREF_KEY_HT_ARG_COUNTERS = "htArgCounters",
    PREF_KEY_HT_CONTENTS_DLG = "htContentsDlg",
    PREF_KEY_HT_DEBATE_PARENTS = "htDebateParents",
    PREF_KEY_HT_DEBATE_POS = "htDebatePos",
    PREF_KEY_HT_DEBATE_SUB = "htDebateSub",
    PREF_KEY_HT_FM_RECORDS = "htFmRecords",
    PREF_KEY_HT_FILE_AUTHORS = "htFileAuthors",
    PREF_KEY_HT_FILE_LABELS = "htFileLabels",
    PREF_KEY_HT_FILE_MENTIONERS = "htFileMentioners",
    PREF_KEY_HT_INST_SUB = "htInstSub",
    PREF_KEY_HT_INST_PEOPLE = "htInstPeople",
    PREF_KEY_HT_FIND = "htFind",
    PREF_KEY_HT_NOTE_PARENTS = "htNoteParents",
    PREF_KEY_HT_NOTE_SUB = "htNoteSub",
    PREF_KEY_HT_NOTE_MENTIONERS = "htNoteMentioners",
    PREF_KEY_HT_PERSON_INST = "htPersonInst",
    PREF_KEY_HT_PERSON_WORKS = "htPersonWorks",
    PREF_KEY_HT_PERSON_ARG = "htPersonArg",
    PREF_KEY_HT_POS_PARENTS = "htPosParents",
    PREF_KEY_HT_POS_ARG = "htPosArg",
    PREF_KEY_HT_POS_SUB = "htPosSub",
    PREF_KEY_HT_QUERY_FIELDS = "htQueryFields",
    PREF_KEY_HT_TERM_GLOSSARIES = "htTermGlossaries",
    PREF_KEY_HT_TERM_DISPLAYERS = "htTermDisplayers",
    PREF_KEY_HT_WORK_AUTHORS = "htWorkAuthors",
    PREF_KEY_HT_WORK_DLG = "htWorkDlg",
    PREF_KEY_HT_WORK_LABELS = "htWorkLabels",
    PREF_KEY_HT_WORK_SUB = "htWorkSub",
    PREF_KEY_HT_WORK_MENTIONERS = "htWorkMentioners",
    PREF_KEY_HT_WORK_INV = "htWorkInv",
    PREF_KEY_HT_WORK_ARG = "htWorkArg",
    PREF_KEY_HT_WORK_FILES = "htWorkFiles",
    PREF_KEY_HT_WORK_MISC = "htWorkMisc",
    PREF_KEY_HT_MGR_FILES = "htMgrFiles",
    PREF_KEY_HT_BIB_ENTRIES = "htBibEntries",

    PREF_KEY_MAINTEXT_ZOOM = "mainTextZoom",
    PREF_KEY_BIBMGR_ZOOM = "bibMgrZoom",
    PREF_KEY_FILEMGR_ZOOM = "fileMgrZoom",
    PREF_KEY_QUERYTAB_ZOOM = "queryTabZoom",
    PREF_KEY_TREETAB_ZOOM = "treeTabZoom",

    PREF_KEY_WINDOW_X = "windowPositionX",
    PREF_KEY_WINDOW_Y = "windowPositionY",
    PREF_KEY_WINDOW_WIDTH = "windowWidth",
    PREF_KEY_WINDOW_HEIGHT = "windowHeight",
    PREF_KEY_WINDOW_ICONIFIED = "windowIconified",
    PREF_KEY_WINDOW_FULLSCREEN = "windowFullScreen",
    PREF_KEY_WINDOW_MAXIMIZED = "windowMaximized",

    PREF_KEY_PREV_WINDOW_X = "prevWindowPositionX",
    PREF_KEY_PREV_WINDOW_Y = "prevWindowPositionY",
    PREF_KEY_PREV_WINDOW_WIDTH = "prevWindowWidth",
    PREF_KEY_PREV_WINDOW_HEIGHT = "prevWindowHeight",

    PREF_KEY_CONTENTS_WINDOW_X = "contentsWindowPositionX",
    PREF_KEY_CONTENTS_WINDOW_Y = "contentsWindowPositionY",
    PREF_KEY_CONTENTS_WINDOW_WIDTH = "contentsWindowWidth",
    PREF_KEY_CONTENTS_WINDOW_HEIGHT = "contentsWindowHeight",

    PREF_KEY_BM_WINDOW_X = "bmWindowPositionX",
    PREF_KEY_BM_WINDOW_Y = "bmWindowPositionY",
    PREF_KEY_BM_WINDOW_WIDTH = "bmWindowWidth",
    PREF_KEY_BM_WINDOW_HEIGHT = "bmWindowHeight",

    PREF_KEY_FM_WINDOW_X = "fmWindowPositionX",
    PREF_KEY_FM_WINDOW_Y = "fmWindowPositionY",
    PREF_KEY_FM_WINDOW_WIDTH = "fmWindowWidth",
    PREF_KEY_FM_WINDOW_HEIGHT = "fmWindowHeight",

    PREF_KEY_PERSON_TOP_HORIZ    = "personTopHoriz", // divider positions
    PREF_KEY_PERSON_MID_VERT     = "personMidVert",
    PREF_KEY_PERSON_BOTTOM_VERT  = "personBottomVert",
    PREF_KEY_INST_MID_HORIZ      = "instMidHoriz",
    PREF_KEY_WORK_MID_VERT       = "workMidVert",
    PREF_KEY_WORK_BOTTOM_VERT    = "workBottomVert",
    PREF_KEY_WORK_RIGHT_HORIZ    = "workRightHoriz",
    PREF_KEY_FILE_BOTTOM_VERT    = "fileBottomVert",
    PREF_KEY_FILE_RIGHT_HORIZ    = "fileRightHoriz",
    PREF_KEY_FILE_RIGHT_VERT     = "fileRightVert",
    PREF_KEY_DEBATE_TOP_VERT     = "debateTopVert",
    PREF_KEY_DEBATE_BOTTOM_VERT  = "debateBottomVert",
    PREF_KEY_DEBATE_BOTTOM_HORIZ = "debateBottomHoriz",
    PREF_KEY_POS_TOP_VERT        = "posTopVert",
    PREF_KEY_POS_BOTTOM_VERT     = "posBottomVert",
    PREF_KEY_POS_BOTTOM_HORIZ    = "posBottomHoriz",
    PREF_KEY_ARG_TOP_VERT        = "argTopVert",
    PREF_KEY_ARG_BOTTOM_VERT     = "argBottomVert",
    PREF_KEY_NOTE_TOP_VERT       = "noteTopVert",
    PREF_KEY_NOTE_BOTTOM_VERT    = "noteBottomVert",
    PREF_KEY_TERM_TOP_VERT       = "termTopVert",
    PREF_KEY_TERM_BOTTOM_VERT    = "termBottomVert",
    PREF_KEY_MGR_MAIN_HORIZ      = "mgrMainHoriz",
    PREF_KEY_MGR_FILES_VERT      = "mgrFilesVert",
    PREF_KEY_MGR_RECORDS_HORIZ   = "mgrRecordsHoriz",
    PREF_KEY_BIB_LEFT_HORIZ      = "bibLeftHoriz",
    PREF_KEY_BIB_RIGHT_HORIZ     = "bibRightHoriz",

    PREF_KEY_FN_COMPONENT_1 = "fnComponent1",
    PREF_KEY_FN_COMPONENT_2 = "fnComponent2",
    PREF_KEY_FN_COMPONENT_3 = "fnComponent3",
    PREF_KEY_FN_COMPONENT_4 = "fnComponent4",
    PREF_KEY_FN_COMPONENT_5 = "fnComponent5",

    PREF_KEY_FN_WITHIN_SEP_1 = "fnWithinSep1",
    PREF_KEY_FN_WITHIN_SEP_2 = "fnWithinSep2",
    PREF_KEY_FN_WITHIN_SEP_3 = "fnWithinSep3",
    PREF_KEY_FN_WITHIN_SEP_4 = "fnWithinSep4",
    PREF_KEY_FN_WITHIN_SEP_5 = "fnWithinSep5",

    PREF_KEY_FN_BEFORE_SEP_1 = "fnBeforeSep1",
    PREF_KEY_FN_BEFORE_SEP_2 = "fnBeforeSep2",
    PREF_KEY_FN_BEFORE_SEP_3 = "fnBeforeSep3",
    PREF_KEY_FN_BEFORE_SEP_4 = "fnBeforeSep4",
    PREF_KEY_FN_BEFORE_SEP_5 = "fnBeforeSep5",

    PREF_KEY_FN_AFTER_SEP_1 = "fnAfterSep1",
    PREF_KEY_FN_AFTER_SEP_2 = "fnAfterSep2",
    PREF_KEY_FN_AFTER_SEP_3 = "fnAfterSep3",
    PREF_KEY_FN_AFTER_SEP_4 = "fnAfterSep4",
    PREF_KEY_FN_AFTER_SEP_5 = "fnAfterSep5",

    PREF_KEY_FN_TEST_1 = "fnTest1",
    PREF_KEY_FN_TEST_2 = "fnTest2",
    PREF_KEY_FN_TEST_3 = "fnTest3",
    PREF_KEY_FN_TEST_4 = "fnTest4",
    PREF_KEY_FN_TEST_5 = "fnTest5",

    PREF_KEY_FN_TREAT_ED_AS_AUTHOR = "fnTreatEdAsAuthor",
    PREF_KEY_FN_ADD_INITIAL = "fnAddInitial",
    PREF_KEY_FN_YEAR_LETTER = "fnYearLetter",
    PREF_KEY_FN_POSIX = "fnPosix",
    PREF_KEY_FN_LOWERCASE = "fnLowercase",
    PREF_KEY_FN_MAX_CHAR = "fnMaxChar",

    PREF_KEY_BIB_LIBRARY_VERSION = "bibLibraryVersion",
    PREF_KEY_BIB_LAST_SYNC_TIME = "bibLastSyncTime",
    PREF_KEY_BIB_API_KEY = "bibApiKey",
    PREF_KEY_BIB_USER_ID = "bibUserID",
    PREF_KEY_BIB_ACCESS_TOKEN = "bibAccessToken",
    PREF_KEY_BIB_REFRESH_TOKEN = "bibRefreshToken",
    PREF_KEY_BIB_LIBRARY_TYPE = "bibType",

    PREF_KEY_KEY_WORK_SORT_BY_NAME = "keyWorkSortByName",
    PREF_KEY_RIGHT_CLICK_TO_LAUNCH = "rtClkToLaunch",

    PREF_KEY_WEB_BUTTONS = "webButtons",

    PREF_KEY_PERSON_SRCH = "personSrch",
    PREF_KEY_WORK_SRCH = "workSrch",
    PREF_KEY_GEN_SRCH = "genSrch",
    PREF_KEY_PERSON_IMG_SRCH = "personImgSrch",
    PREF_KEY_INST_SRCH = "instSrch",
    PREF_KEY_INST_MAP_SRCH = "instMapSrch",
    PREF_KEY_DOI_SRCH = "doiSrch",
    PREF_KEY_ISBN_SRCH = "isbnSrch";

  public static final VersionNumber dbVersion = new VersionNumber(1, 19, 4); // 1.19.4

  // This is the minimum version that the application version is able to load
  public static final Map<VersionNumber, VersionNumber> appVersionToMinRecordsXMLVersion = new ImmutableMap.Builder<VersionNumber, VersionNumber>()

    .put(new VersionNumber(1), new VersionNumber(1))
    .build();

  // This is the minimum version that the application version is able to load
  public static final Map<VersionNumber, VersionNumber> appVersionToMinSettingsXMLVersion = new ImmutableMap.Builder<VersionNumber, VersionNumber>()

    .put(new VersionNumber(1), new VersionNumber(1))
    .build();

  // This is the version that the application version will actually save to
  public static final Map<VersionNumber, VersionNumber> appVersionToMaxRecordsXMLVersion = new ImmutableMap.Builder<VersionNumber, VersionNumber>()

    .put(new VersionNumber(1, 17, 5), new VersionNumber(1))
    .put(dbVersion, new VersionNumber(1, 1))
    .build();

  // This is the version that the application version will actually save to
  public static final Map<VersionNumber, VersionNumber> appVersionToMaxSettingsXMLVersion = new ImmutableMap.Builder<VersionNumber, VersionNumber>()

    .put(new VersionNumber(1, 17, 5), new VersionNumber(1))
    .put(dbVersion, new VersionNumber(1, 1))
    .build();
}
