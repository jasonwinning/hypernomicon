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

package org.hypernomicon.util;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.apache.commons.lang3.Strings;
import org.apache.tika.config.TikaConfig;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.TikaCoreProperties;
import org.apache.tika.mime.MediaType;
import org.apache.tika.mime.MimeTypeException;

import org.hypernomicon.App;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;

import javafx.embed.swing.SwingFXUtils;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.Clipboard;

//---------------------------------------------------------------------------

public final class MediaUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static volatile TikaConfig tikaConfig;

  private MediaUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  /**
   * Provides access to the singleton TikaConfig instance.
   * @return the singleton TikaConfig instance
   */
  private static TikaConfig getTikaConfig()
  {
    if (tikaConfig == null)
    {
      synchronized (MediaUtil.class)
      {
        if (tikaConfig == null)
          tikaConfig = TikaConfig.getDefaultConfig();
      }
    }

    return tikaConfig;
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
      return getTikaConfig().getDetector().detect(stream, metadata);
    }
    catch (IOException e)
    {
      return MediaType.OCTET_STREAM;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int BUFFER_SIZE = 8192; // Buffer size for reading chunks
  private static final int MAX_BYTES_TO_CHECK = 8192 * 128; // 1MB limit; if first MB is ASCII, assume whole file is

  public static boolean isAsciiFile(FilePath filePath)
  {
    try (BufferedInputStream bis = new BufferedInputStream(new FileInputStream(filePath.toFile()), BUFFER_SIZE))
    {
      byte[] buffer = new byte[BUFFER_SIZE];
      int bytesRead,
          totalNonAsciiCount = 0,
          totalBytesRead = 0;

      while ((bytesRead = bis.read(buffer)) != -1)
      {
        int nonAsciiCount = 0;

        for (int i = 0; i < bytesRead; i++)
          if ((buffer[i] & 0xFF) > 127)
          {
            nonAsciiCount++;
            totalNonAsciiCount++;
          }

        // Allow 1 percent or less of non-ASCII characters

        double ratio;

        if (bytesRead >= BUFFER_SIZE)
        {
          ratio = ((double)nonAsciiCount)/((double)bytesRead);
          if (ratio > .01)
            return false;
        }

        totalBytesRead += bytesRead;

        ratio = ((double)totalNonAsciiCount)/((double)totalBytesRead);
        if (ratio > .01)
          return false;

        if (totalBytesRead >= MAX_BYTES_TO_CHECK)
          return true; // Stop after checking 1 MB; if first MB is ASCII, assume whole file is
      }
    }
    catch (IOException e)
    {
      return false;
    }

    return true; // All checked characters are within the ASCII range
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts an image file at the given relative path to a Data URI.
   * Shows a popup window if there is an error.
   *
   * @param relativePath the relative path of the image file
   * @return a Data URI string representing the image, or an empty string if an error occurs
   */
  public static String imgDataURI(String relativePath)
  {
    try (InputStream is = App.class.getResourceAsStream(relativePath))
    {
      if (is == null)
        throw new IOException("Resource not found: " + relativePath);

      try (BufferedInputStream stream = new BufferedInputStream(is);
           ByteArrayOutputStream baos = new ByteArrayOutputStream())
      {
        byte[] buffer = new byte[1024];
        int bytesRead;

        while ((bytesRead = stream.read(buffer)) != -1)
          baos.write(buffer, 0, bytesRead);

        return "data:image/png;base64," + Base64.getEncoder().encodeToString(baos.toByteArray());
      }
    }
    catch (IOException e)
    {
      errorPopup("Error: " + getThrowableMessage(e));
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
    try { return getTikaConfig().getMimeRepository().forName(contentType).getExtension(); }
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
    else if (Strings.CI.containsAny(typeStr, "pdf", "postscript", "framemaker"))
      imageName = "document-pdf";
    else if (Strings.CI.containsAny(typeStr, "djv", "book", "epub"))
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
    else if (Strings.CI.containsAny(typeStr, "word", "rtf", "publisher", "mswrite", "writer", "msword", "xps"))
      imageName = "paper";
    else if (Strings.CI.containsAny(typeStr, "excel", "spread", "calc"))
      imageName = "table-sheet";
    else if (Strings.CI.containsAny(typeStr, "power", "presen", "impress"))
      imageName = "from_current_slide";
    else if (Strings.CI.containsAny(typeStr, "archi", "packa", "install", "diskimage"))
      imageName = "vise-drawer";
    else if (Strings.CI.containsAny(typeStr, "compress", "stuffit", "x-tar", "zip", "x-gtar", "lzma", "lzop", "x-xz"))
      imageName = "vise-drawer";
    else if (typeStr.contains("note"))
      imageName = "notebook-pencil";
    else if (Strings.CI.containsAny(typeStr, "chart", "ivio"))
      imageName = "chart";
    else if (typeStr.endsWith("eps") || Strings.CI.containsAny(typeStr, "emf", "wmf", "cgm", "corel", "kontour", "freehand", "msmetafile",
                                                                         "dwg", "cmx", "cdr", "draw" , "karbon" , "vector"  , "illustr"))
      imageName = "page_white_vector";
    else if (Strings.CI.containsAny(typeStr, "formul", "math"))
      imageName = "edit_mathematics";
    else if (Strings.CI.containsAny(typeStr, "graphic", "image"))
      imageName = "image";
    else if ("audio".equals(mimetype.getType()))
      imageName = "sound_wave";
    else if ("video".equals(mimetype.getType()) || Strings.CI.containsAny(typeStr, "flash", "mp4"))
      imageName = "recording";
    else if (Strings.CI.containsAny(typeStr, "text", "docu"))
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

        return switch (workType)
        {
          case wtBook         -> "resources/images/book.png";
          case wtChapter      -> "resources/images/chapter.png";
          case wtPaper        -> "resources/images/paper.png";
          case wtRecording    -> "resources/images/recording.png";
          case wtThesis       -> "resources/images/thesis.png";
          case wtWebPage      -> "resources/images/text-html.png";
          case wtUnenteredSet -> "resources/images/inbox-document-text.png";
          default             -> "resources/images/unknown.png";
        };

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
    return type == null ? "" : switch (type)
    {
      case hdtWorkLabel     -> labelImgPath;
      case hdtMiscFile      -> "resources/images/file.png";
      case hdtGlossary      -> glossaryImgPath;
      case hdtConcept,
           hdtTerm          -> termImgPath;
      case hdtNote          -> noteImgPath;
      case hdtWork,
           hdtWorkFile      -> "resources/images/paper.png";
      case hdtPerson        -> personImgPath;
      case hdtInstitution   -> institutionImgPath;
      case hdtDebate        -> debateImgPath;
      case hdtPosition      -> positionImgPath;
      case hdtArgument      -> argumentImgPath;
      case hdtInvestigation -> investigationImgPath;
      case hdtFolder        -> folderImgPath;
      default               -> "";
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static RecordType imgRelPathToType(String relPath)
  {
    return strNullOrBlank(relPath) ? hdtNone : switch (relPath)
    {
      case labelImgPath         -> hdtWorkLabel;
      case glossaryImgPath      -> hdtGlossary;
      case termImgPath          -> hdtTerm;
      case noteImgPath          -> hdtNote;
      case personImgPath        -> hdtPerson;
      case institutionImgPath   -> hdtInstitution;
      case debateImgPath        -> hdtDebate;
      case positionImgPath      -> hdtPosition;
      case argumentImgPath      -> hdtArgument;
      case investigationImgPath -> hdtInvestigation;
      case folderImgPath        -> hdtFolder;

      default                   -> hdtNone;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int compareImgRelPaths(String path1, String path2)
  {
    int strCompResult = safeStr(path1).compareTo(safeStr(path2));
    if (strCompResult == 0) return 0;

    RecordType type1 = imgRelPathToType(path1),
               type2 = imgRelPathToType(path2);

    if ((type1 != hdtNone) && (type2 == hdtNone)) return type1  .compareTo(hdtWork);
    if ((type2 != hdtNone) && (type1 == hdtNone)) return hdtWork.compareTo(type2  );

    return type1 == hdtNone ? strCompResult : type1.compareTo(type2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Retrieves an image on the clipboard, converts it to 24-bit so that it can
   * be saved as JPG, and returns a BufferedImage containing the image.
   * @return BufferedImage, or null if the clipboard did not contain an image.
   */
  public static BufferedImage convertClipboardImageTo24BitBuffer()
  {
    Clipboard clipboard = Clipboard.getSystemClipboard();

    if (clipboard.hasImage() == false)
      return null;

    // All this business with the 2 different BufferedImages is because the OpenJDK
    // jpg plugin doesn't support 32-bit color. So we have to convert to 24-bit
    // before writing to jpg.

    Image image = clipboard.getImage();
    if (image == null) return null;

    BufferedImage input = SwingFXUtils.fromFXImage(image, null);
    if (input == null) return null;

    int width  = input.getWidth (),
        height = input.getHeight();

    BufferedImage output = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    int[] px = input.getRGB(0, 0, width, height, null, 0, width);
    output.setRGB(0, 0, width, height, px, 0, width);

    return output;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void readResourceTextFile(String relPath, StringBuilder strBuilder, boolean keepEOLchars) throws IOException
  {
    assignSB(strBuilder, "");

    try (BufferedReader reader = new BufferedReader(new InputStreamReader(App.class.getResourceAsStream(relPath), StandardCharsets.UTF_8)))
    {
      String line;

      while ((line = reader.readLine()) != null)
      {
        if (keepEOLchars && (strBuilder.length() > 0))
          strBuilder.append('\n');

        strBuilder.append(line);
      }
    }
    catch (NullPointerException e)
    {
      throw new IOException(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
