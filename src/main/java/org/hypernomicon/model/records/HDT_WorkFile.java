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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

public class HDT_WorkFile extends HDT_Record implements HDT_RecordWithPath
{
  private final HyperPath path;
  public final List<HDT_Work> works;

  public HDT_WorkFile(HDT_RecordState xmlState, HyperDataset<HDT_WorkFile> dataset)
  {
    super(xmlState, dataset, tagName);

    works = getSubjList(rtWorkFileOfWork);
    path = new HyperPath(getObjPointer(rtFolderOfWorkFile), this);
  }

  @Override public String listName()        { return path.getNameStr(); }
  @Override public HDT_RecordType getType() { return hdtWorkFile; }
  @Override public HyperPath getPath()      { return path; }

  public boolean getAnnotated()         { return getTagBoolean(tagAnnotated); }
  public void setAnnotated(boolean val) { updateTagBoolean(tagAnnotated, val); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    path.clear();
    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class FileNameAuthor
  {
    public String name;
    public boolean isEditor;
    public boolean isTrans;

    public FileNameAuthor(String name, boolean isEditor, boolean isTrans)
    {
      this.name = name;
      this.isEditor = isEditor;
      this.isTrans = isTrans;
    }
  }

//---------------------------------------------------------------------------

  public static class FileNameComponentConfig
  {
    public int code;
    public String beforeSep, withinSep, afterSep;

    public FileNameComponentConfig(int code, String beforeSep, String withinSep, String afterSep)
    {
      this.code = code;
      this.beforeSep = beforeSep;
      this.withinSep = withinSep;
      this.afterSep = afterSep;
    }
  }

//---------------------------------------------------------------------------

  public static String makeFileName(List<FileNameAuthor> authors, String year, String title, String ext)
  {
    ArrayList<FileNameComponentConfig> configList = new ArrayList<>();
    FileNameComponentConfig authConfig = null;

    String fileName = "";

    configList.add(new FileNameComponentConfig(db.prefs.getInt(PREF_KEY_FN_COMPONENT_1, BLANK_FN_COMPONENT),
                                               db.prefs.get(PREF_KEY_FN_BEFORE_SEP_1, ""),
                                               db.prefs.get(PREF_KEY_FN_WITHIN_SEP_1, " "),
                                               db.prefs.get(PREF_KEY_FN_AFTER_SEP_1, "")));

    configList.add(new FileNameComponentConfig(db.prefs.getInt(PREF_KEY_FN_COMPONENT_2, BLANK_FN_COMPONENT),
                                               db.prefs.get(PREF_KEY_FN_BEFORE_SEP_2, ""),
                                               db.prefs.get(PREF_KEY_FN_WITHIN_SEP_2, " "),
                                               db.prefs.get(PREF_KEY_FN_AFTER_SEP_2, "")));

    configList.add(new FileNameComponentConfig(db.prefs.getInt(PREF_KEY_FN_COMPONENT_3, BLANK_FN_COMPONENT),
                                               db.prefs.get(PREF_KEY_FN_BEFORE_SEP_3, ""),
                                               db.prefs.get(PREF_KEY_FN_WITHIN_SEP_3, " "),
                                               db.prefs.get(PREF_KEY_FN_AFTER_SEP_3, "")));

    configList.add(new FileNameComponentConfig(db.prefs.getInt(PREF_KEY_FN_COMPONENT_4, BLANK_FN_COMPONENT),
                                               db.prefs.get(PREF_KEY_FN_BEFORE_SEP_4, ""),
                                               db.prefs.get(PREF_KEY_FN_WITHIN_SEP_4, " "),
                                               db.prefs.get(PREF_KEY_FN_AFTER_SEP_4, "")));

    configList.add(new FileNameComponentConfig(db.prefs.getInt(PREF_KEY_FN_COMPONENT_5, BLANK_FN_COMPONENT),
                                               db.prefs.get(PREF_KEY_FN_BEFORE_SEP_5, ""),
                                               db.prefs.get(PREF_KEY_FN_WITHIN_SEP_5, " "),
                                               db.prefs.get(PREF_KEY_FN_AFTER_SEP_5, "")));

    for (FileNameComponentConfig config : configList)
    {
      if (config.code == AUTHOR_FN_COMPONENT)
      {
        authConfig = config;
        break;
      }
    }

    for (FileNameComponentConfig config : configList)
      fileName = fileName + getFNComponent(config, authConfig, authors, year, title);

    fileName = fileName.trim();

    if (db.prefs.getBoolean(PREF_KEY_FN_POSIX, false))
    {
      fileName = convertToEnglishChars(fileName);
      while (fileName.startsWith("-"))
        fileName = fileName.substring(1);

      String newName = "";
      for (int pos = 0; pos < fileName.length(); pos++)
      {
        if (((fileName.charAt(pos) >= 'A') && (fileName.charAt(pos) <= 'Z')) ||
            ((fileName.charAt(pos) >= 'a') && (fileName.charAt(pos) <= 'z')) ||
            ((fileName.charAt(pos) >= '0') && (fileName.charAt(pos) <= '9')) ||
            (fileName.charAt(pos) == '-') ||
            (fileName.charAt(pos) == '_'))
          newName = newName + fileName.charAt(pos);
      }

      fileName = "" + newName;
    }

    if (db.prefs.getBoolean(PREF_KEY_FN_LOWERCASE, false))
      fileName = fileName.toLowerCase();

    fileName = FilePath.removeInvalidFileNameChars(fileName);

    int maxLen = db.prefs.getInt(PREF_KEY_FN_MAX_CHAR, 255);
    int extLen;

    if (ext.length() > 0)
    {
      extLen = ext.length() + FilenameUtils.EXTENSION_SEPARATOR_STR.length();
      if ((fileName.length() + extLen) > maxLen)
        fileName = fileName.substring(0, (maxLen - extLen));

      return fileName.trim() + FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
    }

    if (fileName.length() > maxLen)
      fileName = fileName.substring(0, maxLen);

    return fileName.trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAuthorStr(List<FileNameAuthor> authors, boolean isEditor, boolean isTrans)
  {
    String authorStr, comp = "";
    int pos;

    for (FileNameAuthor author : authors)
    {
      if (((author.isEditor == false) && (author.isTrans == false) && (isEditor == false) && (isTrans == false)) ||
          (author.isEditor && isEditor) ||
          (author.isTrans && isTrans))
      {
        authorStr = "" + author.name;
        pos = authorStr.indexOf(',');

        if (pos >= 0)
          authorStr = authorStr.substring(0, pos);

        comp = comp.length() == 0 ? ("" + authorStr) : (comp + " " + authorStr);
      }
    }

    return comp;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getFNComponent(FileNameComponentConfig config, FileNameComponentConfig authConfig, List<FileNameAuthor> authors, String year, String title)
  {
    String comp = "";
    int pos;
    boolean treatAsAuthor = false;

    switch (config.code)
    {
      case AUTHOR_FN_COMPONENT :

        comp = getAuthorStr(authors, false, false);
        break;

      case EDITOR_FN_COMPONENT :

        if ((db.prefs.getBoolean(PREF_KEY_FN_TREAT_ED_AS_AUTHOR, true)) && (authConfig != null))
        {
          treatAsAuthor = true;
          for (FileNameAuthor author : authors)
          {
            if ((author.isEditor == false) && (author.isTrans == false))
              treatAsAuthor = false;
          }
        }

        if (treatAsAuthor)
          config = authConfig;

        comp = getAuthorStr(authors, true, false);
        break;

      case TRANS_FN_COMPONENT :

        comp = getAuthorStr(authors, false, true);
        break;

      case TITLE_FN_COMPONENT :

        comp = title;
        pos = indexOfAny(":?*|\"<>/\\", comp);

        if (pos >= 0)
          comp = comp.substring(0, pos);

        break;

      case YEAR_FN_COMPONENT :

        comp = year;
        break;

      default :

        comp = "";
        break;
    }

    comp = ultraTrim(comp).replace(" ", config.withinSep);

    if (comp.length() > 0)
      comp = config.beforeSep + comp + config.afterSep;

    return comp;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
