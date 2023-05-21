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

package org.hypernomicon.util;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.io.BufferedInputStream;
import java.io.IOException;

import org.apache.tika.config.TikaConfig;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.TikaCoreProperties;
import org.apache.tika.mime.MediaType;
import org.apache.tika.mime.MimeTypeException;
import org.hypernomicon.App;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;

import javafx.scene.image.ImageView;

public final class MediaUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TikaConfig tikaConfig;

  private MediaUtil() { throw new UnsupportedOperationException(); }

//---------------------------------------------------------------------------

  public static void init()
  {
    tikaConfig = TikaConfig.getDefaultConfig();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MediaType getMediaType(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return MediaType.OCTET_STREAM;

    Metadata metadata = new Metadata();
    metadata.set(TikaCoreProperties.RESOURCE_NAME_KEY, filePath.toString());

    try (TikaInputStream stream = TikaInputStream.get(filePath.toPath()))
    {
      return tikaConfig.getDetector().detect(stream, metadata);
    }
    catch (IOException e)
    {
      return MediaType.OCTET_STREAM;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String imgDataURI(String relativePath)
  {
    try (BufferedInputStream stream = new BufferedInputStream(App.class.getResourceAsStream(relativePath)))
    {
      byte[] array = new byte[stream.available()];

      noOp(stream.read(array));

      return "data:image/png;base64," + javax.xml.bind.DatatypeConverter.printBase64Binary(array);
    }
    catch (IOException e)
    {
      messageDialog("Error: " + e.getMessage(), mtError);
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String imgDataURIbyRecordType (RecordType type  ) { return imgDataURI(imgRelPathByType(type  )); }
  public static String imgDataURIbyRecord     (HDT_Record record) { return imgDataURI(imgRelPath      (record)); }

  public static ImageView imgViewForRecordType(RecordType type  ) { return imgViewFromRelPath(imgRelPathByType(type  )); }
  public static ImageView imgViewForRecord    (HDT_Record record) { return imgViewFromRelPath(imgRelPath      (record)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView imgViewFromRelPath(String relPath)
  {
    return relPath.isEmpty() ? null : new ImageView(App.class.getResource(relPath).toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getContentTypeExtension(String contentType)
  {
    try { return tikaConfig.getMimeRepository().forName(contentType).getExtension(); }
    catch (MimeTypeException e) { return ""; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView imgViewFromFilePath(FilePath filePath, MediaType mediaType)
  {
    return imgViewFromRelPath(imgRelPath(filePath, mediaType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String imgRelPath(FilePath filePath, MediaType mimetype)
  {
    if (mimetype == null)
      mimetype = getMediaType(filePath);

    String imageName, typeStr = mimetype.toString();

    if (mimetype == MediaType.APPLICATION_XML)
      imageName = "document-code";
    else if (mimetype == MediaType.APPLICATION_ZIP)
      imageName = "vise-drawer";
    else if (strContainsAnyStr(typeStr, "pdf", "postscript", "framemaker"))
      imageName = "document-pdf";
    else if (strContainsAnyStr(typeStr, "djv", "book", "epub"))
      imageName = "book";
    else if ("image".equals(mimetype.getType()))
      imageName = "image";
    else if (typeStr.contains("plain"))
      imageName = "document-text";
    else if (typeStr.contains("htm"))
      imageName = "text-html";
    else if (typeStr.contains("json"))
      imageName = "json";
    else if (typeStr.endsWith("tex"))
      imageName = "document-tex";
    else if (strContainsAnyStr(typeStr, "word", "rtf", "publisher", "mswrite", "writer", "msword", "xps"))
      imageName = "paper";
    else if (strContainsAnyStr(typeStr, "excel", "spread", "calc"))
      imageName = "table-sheet";
    else if (strContainsAnyStr(typeStr, "power", "presen", "impress"))
      imageName = "from_current_slide";
    else if (strContainsAnyStr(typeStr, "archi", "packa", "install", "diskimage"))
      imageName = "vise-drawer";
    else if (strContainsAnyStr(typeStr, "compress", "stuffit", "x-tar", "zip", "x-gtar", "lzma", "lzop", "x-xz"))
      imageName = "vise-drawer";
    else if (typeStr.contains("note"))
      imageName = "notebook-pencil";
    else if (strContainsAnyStr(typeStr, "chart", "ivio"))
      imageName = "chart";
    else if (typeStr.endsWith("eps") || strContainsAnyStr(typeStr, "emf", "wmf", "cgm", "corel", "kontour", "freehand", "msmetafile",
                                                                   "dwg", "cmx", "cdr", "draw" , "karbon" , "vector"  , "illustr"))
      imageName = "page_white_vector";
    else if (strContainsAnyStr(typeStr, "formul", "math"))
      imageName = "edit_mathematics";
    else if (strContainsAnyStr(typeStr, "graphic", "image"))
      imageName = "image";
    else if ("audio".equals(mimetype.getType()))
      imageName = "sound_wave";
    else if ("video".equals(mimetype.getType()) || strContainsAnyStr(typeStr, "flash", "mp4"))
      imageName = "recording";
    else if (strContainsAnyStr(typeStr, "text", "docu"))
      imageName = "document-text";
    else
      imageName = "document";

    return "resources/images/" + imageName + ".png";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String imgRelPath(HDT_Record record)
  {
    switch (record.getType())
    {
      case hdtWork :

        WorkTypeEnum workType = ((HDT_Work) record).getWorkTypeEnum();

        switch (workType)
        {
          case wtBook         : return "resources/images/book.png";
          case wtChapter      : return "resources/images/chapter.png";
          case wtPaper        : return "resources/images/paper.png";
          case wtRecording    : return "resources/images/recording.png";
          case wtThesis       : return "resources/images/thesis.png";
          case wtWebPage      : return "resources/images/text-html.png";
          case wtUnenteredSet : return "resources/images/inbox-document-text.png";
          default             : return "resources/images/unknown.png";
        }

      case hdtMiscFile :

        HDT_MiscFile miscFile = (HDT_MiscFile) record;

        if (miscFile.pathNotEmpty())
          return imgRelPath(miscFile.filePath(), null);

        // Fall through

      default :

        return imgRelPathByType(record.getType());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String labelImgPath         = "resources/images/tag.png",
                              glossaryImgPath      = "resources/images/bookshelf.png",
                              termImgPath          = "resources/images/term.png",
                              noteImgPath          = "resources/images/notebook-pencil.png",
                              personImgPath        = "resources/images/people.png",
                              institutionImgPath   = "resources/images/building-hedge.png",
                              debateImgPath        = "resources/images/debate.png",
                              positionImgPath      = "resources/images/position.png",
                              argumentImgPath      = "resources/images/argument.png",
                              investigationImgPath = "resources/images/documents-stack.png",
                              folderImgPath        = "resources/images/folder.png";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String imgRelPathByType(RecordType type)
  {
    if (type == null) return "";

    switch (type)
    {
      case hdtWorkLabel     : return labelImgPath;
      case hdtMiscFile      : return "resources/images/file.png";
      case hdtGlossary      : return glossaryImgPath;
      case hdtConcept       : // Fall through
      case hdtTerm          : return termImgPath;
      case hdtNote          : return noteImgPath;
      case hdtWork          : // Fall through
      case hdtWorkFile      : return "resources/images/paper.png";
      case hdtPerson        : return personImgPath;
      case hdtInstitution   : return institutionImgPath;
      case hdtDebate        : return debateImgPath;
      case hdtPosition      : return positionImgPath;
      case hdtArgument      : return argumentImgPath;
      case hdtInvestigation : return investigationImgPath;
      case hdtFolder        : return folderImgPath;
      default               : return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static RecordType imgRelPathToType(String relPath)
  {
    if (safeStr(relPath).isBlank()) return hdtNone;

    switch (relPath)
    {
      case labelImgPath         : return hdtWorkLabel;
      case glossaryImgPath      : return hdtGlossary;
      case termImgPath          : return hdtTerm;
      case noteImgPath          : return hdtNote;
      case personImgPath        : return hdtPerson;
      case institutionImgPath   : return hdtInstitution;
      case debateImgPath        : return hdtDebate;
      case positionImgPath      : return hdtPosition;
      case argumentImgPath      : return hdtArgument;
      case investigationImgPath : return hdtInvestigation;
      case folderImgPath        : return hdtFolder;

      default                   : return hdtNone;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int compareImgRelPaths(String path1, String path2)
  {
    int strCompResult = safeStr(path1).compareTo(safeStr(path2));
    if (strCompResult == 0) return 0;

    RecordType type1 = imgRelPathToType(path1),
               type2 = imgRelPathToType(path2);

    if ((type1 != hdtNone) && (type2 == hdtNone)) return type1.compareTo(hdtWork);
    if ((type2 != hdtNone) && (type1 == hdtNone)) return hdtWork.compareTo(type2);

    return type1 == hdtNone ? strCompResult : type1.compareTo(type2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
