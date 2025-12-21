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

package org.hypernomicon.util;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

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
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;

import javafx.scene.image.ImageView;

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

  public static String imgDataURIbyRecord(HDT_Record record) { return imgDataURI(imgRelPath(record, record.getType(), null)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView imgViewForRecord(HDT_Record record, RecordType type)
  {
    return imgViewForRecord(record, type, null);
  }

  public static ImageView imgViewForRecord(HDT_Record record, RecordType type, HDT_Record contextRecord)
  {
    return imgViewFromRelPath(imgRelPath(record, type, contextRecord));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView imgViewFromRelPath(String relPath)
  {
    return strNullOrEmpty(relPath) ? null : new ImageView(App.class.getResource(relPath).toString());
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

  /**
   * Get the relative path to the image resource for a record or record type.
   * <p>
   * If <code>record</code> is null, it will go according to the type.
   * <p>
   * Otherwise, it will use the record's data and (optionally) an additional
   * record that provides context to determine the image.
   * <p>
   * For example, the contextRecord for an Argument record might be the
   * record (Position or Argument) targeted by the Argument.
   * @param record The record whose image to determine.
   * @param recordType The record type whose image to determine.
   * @param contextRecord Record providing context.
   * @return Relative path to the image resource.
   */
  public static String imgRelPath(HDT_Record record, RecordType recordType, HDT_Record contextRecord)
  {
    if (record != null)
    {
      recordType = record.getType();

      switch (recordType)
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

        case hdtArgument :
        {
          Ternary inFavor = null;
          HDT_Verdict verdict = null;

          HDT_Argument argument = (HDT_Argument) record;

          if (contextRecord instanceof HDT_Position position)
            verdict = argument.getPosVerdict(position);

          if ((verdict == null) && (contextRecord instanceof HDT_Argument targetArg))
            verdict = argument.getArgVerdict(targetArg);

          if (verdict != null)
            inFavor = verdict.isInFavor();
          else
          {
            boolean anyFalse = false,
                    anyTrue  = false;

            for (HDT_Position position : argument.positions)
            {
              Ternary curInFavor = argument.isInFavor(position);

              if      (curInFavor.isTrue ()) anyTrue  = true;
              else if (curInFavor.isFalse()) anyFalse = true;
            }

            for (HDT_Argument targetArg : argument.targetArgs)
            {
              Ternary curInFavor = argument.isInFavor(targetArg);

              if      (curInFavor.isTrue ()) anyTrue  = true;
              else if (curInFavor.isFalse()) anyFalse = true;
            }

            if      (anyTrue  && (anyFalse == false)) inFavor = Ternary.True;
            else if (anyFalse && (anyTrue  == false)) inFavor = Ternary.False;
          }

          if (argument.getIsArgument().isFalse())
          {
            if (Ternary.isNullOrUnset(inFavor)) return "resources/images/stance.png";
            if (inFavor.isTrue())               return "resources/images/stance-for.png";

            return "resources/images/stance-against.png";
          }

          if (Ternary.isNullOrUnset(inFavor)) return "resources/images/argument.png";
          if (inFavor.isTrue())               return "resources/images/argument-for.png";

          return "resources/images/argument-against.png";
        }
        case hdtMiscFile :

          HDT_MiscFile miscFile = (HDT_MiscFile) record;

          if (miscFile.pathNotEmpty())
            return imgRelPath(miscFile.filePath(), null);

          break;

        default :

          break;
      }
    }

    return recordType == null ? "" : switch (recordType)
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
