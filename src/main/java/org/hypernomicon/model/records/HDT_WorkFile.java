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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.settings.WorkFileNamingSettingsCtrlr.WorkFileNameComponent;
import org.hypernomicon.settings.WorkFileNamingSettingsCtrlr.WorkFileNameComponentType;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import java.util.EnumSet;
import java.util.List;

public class HDT_WorkFile extends HDT_RecordBase implements HDT_RecordWithPath
{
  private final HyperPath path;
  public final List<HDT_Work> works;

  public HDT_WorkFile(RecordState xmlState, HyperDataset<HDT_WorkFile> dataset)
  {
    super(xmlState, dataset, tagName);

    works = getSubjList(rtWorkFileOfWork);
    path = new HyperPath(getObjPointer(rtFolderOfWorkFile), this);
  }

  @Override public HyperPath getPath()                 { return path; }
  @Override public String listName()                   { return path.getNameStr(); }
  @Override protected String makeSortKeyTypeSpecific() { return path.getNameStr(); }

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
    private final String name;
    private final boolean isEditor, isTrans;

    public FileNameAuthor(String name, boolean isEditor, boolean isTrans)
    {
      this.name = name;
      this.isEditor = isEditor;
      this.isTrans = isTrans;
    }
  }

//---------------------------------------------------------------------------

  public static String makeFileName(List<FileNameAuthor> authors, HDT_WorkType workType, String year, String title, String container, String publisher, String ext)
  {
    return makeFileName(authors, workType, year, title, container, publisher, ext, null);
  }

  public static String makeFileName(List<FileNameAuthor> authors, HDT_WorkType workType, String year, String title, String container, String publisher, String ext, List<WorkFileNameComponent> components)
  {
    if (components == null)
      components = WorkFileNameComponent.loadFromPrefs();

    WorkFileNameComponent authComponent = findFirst(components, component -> component.type == WorkFileNameComponentType.fncAuthorLastNames);
    String fileName = "";

    EnumSet<WorkFileNameComponentType> usedComponentTypes = EnumSet.noneOf(WorkFileNameComponentType.class);

    for (WorkFileNameComponent component : components)
    {
      if ((component.type == WorkFileNameComponentType.fncBlank) || (component.type == null) || usedComponentTypes.contains(component.type) || ((workType != null) && component.excludedWorkTypes.contains(workType)))
        continue;

      fileName = fileName + getFNComponent(component, authComponent, authors, year, title, container, publisher);
      usedComponentTypes.add(component.type);
    }

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

      fileName = newName;
    }

    if (db.prefs.getBoolean(PREF_KEY_FN_LOWERCASE, false))
      fileName = fileName.toLowerCase();

    fileName = FilePath.removeInvalidFileNameChars(fileName);

    int maxLen = db.prefs.getInt(PREF_KEY_FN_MAX_CHAR, 255);

    if (ext.length() > 0)
    {
      int extLen = ext.length() + FilenameUtils.EXTENSION_SEPARATOR_STR.length();
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

  private static String getAuthorStr(Iterable<FileNameAuthor> authors, boolean isEditor, boolean isTrans)
  {
    String comp = "";

    for (FileNameAuthor author : authors)
    {
      if (((author.isEditor == false) && (author.isTrans == false) && (isEditor == false) && (isTrans == false)) ||
          (author.isEditor && isEditor) ||
          (author.isTrans && isTrans))
      {
        String authorStr = author.name;
        int pos = authorStr.indexOf(',');

        if (pos >= 0)
          authorStr = authorStr.substring(0, pos);

        comp = comp.isEmpty() ? authorStr : (comp + ' ' + authorStr);
      }
    }

    return comp;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getFNComponent(WorkFileNameComponent component,  WorkFileNameComponent authComponent, List<FileNameAuthor> authors, String year, String title, String container, String publisher)
  {
    String compStr = "";
    int pos;

    switch (component.type)
    {
      case fncAuthorLastNames :

        compStr = getAuthorStr(authors, false, false);
        break;

      case fncEditors :

        if (db.prefs.getBoolean(PREF_KEY_FN_TREAT_ED_AS_AUTHOR, true) && (authComponent != null) &&
            authors.stream().allMatch(author -> author.isEditor || author.isTrans))
          component = authComponent;

        compStr = getAuthorStr(authors, true, false);
        break;

      case fncTranslators :

        compStr = getAuthorStr(authors, false, true);
        break;

      case fncTitleNoSub : // similar to fncContainerNoSub case

        compStr = title;
        pos = indexOfAny(":?*|\"<>/\\", compStr);

        if (pos >= 0)
          compStr = compStr.substring(0, pos);

        break;

      case fncContainerNoSub: // similar to fncTitleNoSub case

        compStr = container;
        pos = indexOfAny(":?*|\"<>/\\", compStr);

        if (pos >= 0)
          compStr = compStr.substring(0, pos);

        break;

      case fncYear :

        compStr = year;
        break;

      case fncPublisher:

        compStr = publisher;
        break;

      default :

        break;
    }

    compStr = ultraTrim(compStr).replace(" ", component.withinSep);

    if (compStr.length() > 0)
      compStr = component.beforeSep + compStr + component.afterSep;

    return compStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
