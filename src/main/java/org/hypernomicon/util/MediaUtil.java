/*
 * Copyright 2015-2021 Jason Winning
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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Arrays;

import org.apache.tika.config.TikaConfig;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.mime.MediaType;
import org.apache.tika.mime.MimeTypeException;
import org.hypernomicon.App;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;

import com.ibm.icu.text.CharsetDetector;

import javafx.scene.image.ImageView;

public class MediaUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TikaConfig tika;

//---------------------------------------------------------------------------

  public static void init()
  {
    tika = TikaConfig.getDefaultConfig();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MediaType getMediaType(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return MediaType.OCTET_STREAM;

    Metadata metadata = new Metadata();
    metadata.set(Metadata.RESOURCE_NAME_KEY, filePath.toString());

    try (TikaInputStream stream = TikaInputStream.get(filePath.toPath()))
    {
      return tika.getDetector().detect(stream, metadata);
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
    try (BufferedInputStream stream = new BufferedInputStream(App.class.getResourceAsStream(relativePath));)
    {
      byte[] array = new byte[stream.available()];

      stream.read(array);

      return "data:image/png;base64," + javax.xml.bind.DatatypeConverter.printBase64Binary(array);
    }
    catch (Exception e)
    {
      messageDialog("Error: " + e.getMessage(), mtError);
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String imgDataURIbyRecordType (RecordType type      ) { return imgDataURI(imgRelPathByType(type  )); }
  public static String imgDataURIbyRecord     (HDT_Record     record) { return imgDataURI(imgRelPath      (record)); }

  public static ImageView imgViewForRecordType(RecordType type      ) { return imgViewFromRelPath(imgRelPathByType(type  )); }
  public static ImageView imgViewForRecord    (HDT_Record     record) { return imgViewFromRelPath(imgRelPath      (record)); }

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
    try { return tika.getMimeRepository().forName(contentType).getExtension(); }
    catch (MimeTypeException e) { return ""; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView imgViewFromFilePath(FilePath filePath, MediaType mediaType, boolean isDir)
  {
    return imgViewFromRelPath(imgRelPath(filePath, mediaType, isDir));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String imgRelPath(FilePath filePath, MediaType mimetype, boolean isDir)
  {
    if (isDir)
      return "resources/images/folder.png";

    if (mimetype == null)
      mimetype = getMediaType(filePath);

    String imageName = "", typeStr = mimetype.toString();

    if (mimetype == MediaType.APPLICATION_XML)
      imageName = "document-code";
    else if (mimetype == MediaType.APPLICATION_ZIP)
      imageName = "vise-drawer";
    else if (containsAny(typeStr, "pdf", "postscript", "framemaker"))
      imageName = "document-pdf";
    else if (containsAny(typeStr, "djv", "book", "epub"))
      imageName = "book";
    else if (mimetype.getType().equals("image"))
      imageName = "image";
    else if (typeStr.contains("plain"))
      imageName = "document-text";
    else if (typeStr.contains("htm"))
      imageName = "text-html";
    else if (typeStr.contains("json"))
      imageName = "json";
    else if (typeStr.endsWith("tex"))
      imageName = "document-tex";
    else if (containsAny(typeStr, "word", "rtf", "publisher", "mswrite", "writer", "msword", "xps"))
      imageName = "paper";
    else if (containsAny(typeStr, "excel", "spread", "calc"))
      imageName = "table-sheet";
    else if (containsAny(typeStr, "power", "presen", "impress"))
      imageName = "from_current_slide";
    else if (containsAny(typeStr, "archi", "packa", "install", "diskimage"))
      imageName = "vise-drawer";
    else if (containsAny(typeStr, "compress", "stuffit", "x-tar", "zip", "x-gtar", "lzma", "lzop", "x-xz"))
      imageName = "vise-drawer";
    else if (typeStr.contains("note"))
      imageName = "notebook-pencil";
    else if (containsAny(typeStr, "chart", "ivio"))
      imageName = "chart";
    else if (typeStr.endsWith("eps") || containsAny(typeStr, "emf", "wmf", "cgm", "corel", "kontour", "freehand", "msmetafile",
                                                             "dwg", "cmx", "cdr", "draw" , "karbon" , "vector"  , "illustr"))
      imageName = "page_white_vector";
    else if (containsAny(typeStr, "formul", "math"))
      imageName = "edit_mathematics";
    else if (containsAny(typeStr, "graphic", "image"))
      imageName = "image";
    else if (mimetype.getType().equals("audio"))
      imageName = "sound_wave";
    else if (mimetype.getType().equals("video") || containsAny(typeStr, "flash", "mp4"))
      imageName = "recording";
    else if (containsAny(typeStr, "text", "docu"))
      imageName = "document-text";
    else
      imageName = "document";

    return "resources/images/" + imageName + ".png";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean containsAny(String container, String... strings)
  {
    return Arrays.stream(strings).parallel().anyMatch(container::contains);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String imgRelPath(HDT_Record record)
  {
    switch (record.getType())
    {
      case hdtWork :

        WorkTypeEnum workType = HDT_Work.class.cast(record).getWorkTypeEnum();

        switch (workType)
        {
          case wtBook         : return "resources/images/book.png";
          case wtChapter      : return "resources/images/chapter.png";
          case wtNone         : return "resources/images/unknown.png";
          case wtPaper        : return "resources/images/paper.png";
          case wtRecording    : return "resources/images/recording.png";
          case wtWebPage      : return "resources/images/text-html.png";
          case wtUnenteredSet : return "resources/images/inbox-document-text.png";
          default             : return "resources/images/unknown.png";
        }

      case hdtMiscFile :

        HDT_MiscFile miscFile = (HDT_MiscFile) record;

        if (miscFile.pathNotEmpty())
          return imgRelPath(miscFile.filePath(), null, false);

      default :

        return imgRelPathByType(record.getType());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String imgRelPathByType(RecordType type)
  {
    switch (type)
    {
      case hdtWorkLabel     : return "resources/images/tag.png";
      case hdtMiscFile      : return "resources/images/file.png";
      case hdtConcept       : return "resources/images/term.png";
      case hdtGlossary      : return "resources/images/bookshelf.png";
      case hdtTerm          : return "resources/images/term.png";
      case hdtNote          : return "resources/images/notebook-pencil.png";
      case hdtWork          : // Fall through
      case hdtWorkFile      : return "resources/images/paper.png";
      case hdtPerson        : return "resources/images/people.png";
      case hdtInstitution   : return "resources/images/building-hedge.png";
      case hdtDebate        : return "resources/images/debate.png";
      case hdtPosition      : return "resources/images/position.png";
      case hdtArgument      : return "resources/images/argument.png";
      case hdtInvestigation : return "resources/images/documents-stack.png";
      case hdtFolder        : return "resources/images/folder.png";
      default               : return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Charset detectCharset(byte[] byteData)
  {
    CharsetDetector detector = new CharsetDetector();

    detector.setText(byteData);

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Charset detectCharset(InputStream streamData)
  {
    CharsetDetector detector = new CharsetDetector();

    try
    {
      detector.setText(streamData);
    }
    catch (IOException e)
    {
      return null;
    }

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
