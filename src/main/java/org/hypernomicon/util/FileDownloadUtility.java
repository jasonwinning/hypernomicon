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

package org.hypernomicon.util;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.apache.commons.io.FilenameUtils;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.tika.mime.MimeType;
import org.apache.tika.mime.MimeTypeException;

import org.hypernomicon.App;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.util.AsyncHttpClient.ExHandler;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.util.Util.*;

public class FileDownloadUtility
{
  private static final int BUFFER_SIZE = 4096;

  @FunctionalInterface public interface BufferHandler { public void handle(Buffer buffer); }

  public static class Buffer extends InputStream
  {
    private ArrayList<byte[]> buffers = new ArrayList<>();
    private ArrayList<Integer> lengths = new ArrayList<>();
    private int curBufferNdx = 0, curPosition = 0;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public Buffer(InputStream inputStream) throws IOException
    {
      int bytesRead = -1;
      byte[] buffer = new byte[BUFFER_SIZE];
      while ((bytesRead = inputStream.read(buffer)) != -1)
      {
        buffers.add(buffer);
        lengths.add(bytesRead);
        buffer = new byte[BUFFER_SIZE];
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public int length()
    {
      int total = 0;
      for (Integer length : lengths)
        total += length;

      return total;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public void saveToFile(FilePath saveFilePath) throws FileNotFoundException, IOException
    {
      try (FileOutputStream outputStream = new FileOutputStream(saveFilePath.toFile()))
      {
        for (int bufferNdx = 0; bufferNdx < buffers.size(); bufferNdx++)
          outputStream.write(buffers.get(bufferNdx), 0, lengths.get(bufferNdx));
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public void reposition(int pos)
    {
      int origPos = pos;

      for (int bufferNdx = 0; bufferNdx < buffers.size(); bufferNdx++)
      {
        if (pos < lengths.get(bufferNdx))
        {
          curBufferNdx = bufferNdx;
          curPosition = pos;
          return;
        }

        pos = pos - lengths.get(bufferNdx);
      }

      throw new IndexOutOfBoundsException("Length = " + length() + "; index = " + origPos);
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public int read() throws IOException
    {
      byte val;

      if (curPosition >= lengths.get(curBufferNdx).intValue())
      {
        curPosition = 0;
        curBufferNdx++;
      }

      if (curBufferNdx >= buffers.size())
        throw new IOException("End of buffer");

      val = buffers.get(curBufferNdx)[curPosition];
      curPosition++;

      return val + 128;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void downloadToFile(String fileURL, FilePath dirPath, String fileNameStr, StringBuilder fileName, boolean assumeIsImage,
                                    AsyncHttpClient httpClient, BufferHandler successHndlr, ExHandler failHndlr)
  {
    downloadFile(fileURL, dirPath, fileNameStr, false, fileName, assumeIsImage, httpClient, successHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void downloadToBuffer(String fileURL, StringBuilder fileName, boolean assumeIsImage,
                                      AsyncHttpClient httpClient, BufferHandler successHndlr, ExHandler failHndlr)
  {
    downloadFile(fileURL, null, "", true, fileName, assumeIsImage, httpClient, successHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings({ "unused" })
  private static void downloadFile(String fileURL, FilePath dirPath, String fileNameStr, boolean saveToBuffer, StringBuilder fileName,
                                   boolean assumeIsImage, AsyncHttpClient httpClient, BufferHandler successHndlr, ExHandler failHndlr)
  {
    assignSB(fileName, "");

    ResponseHandler<Boolean> responseHndlr = response ->
    {
      String contentType = "", disposition = "";
      int contentLength = -1;
      FilePath saveFilePath;

      int statusCode = response.getStatusLine().getStatusCode();
      String reasonPhrase = response.getStatusLine().getReasonPhrase();

      if (statusCode >= 400)
      {
        runInFXThread(() -> failHndlr.handle(new HttpResponseException(statusCode, reasonPhrase)));
        return false;
      }

      HttpEntity entity = response.getEntity();

      Header[] headers = response.getAllHeaders();

      for (Header header : headers)
      {
        switch (header.getName())
        {
          case "Content-Type" : contentType = header.getValue(); break;
          case "Content-Length" : contentLength = parseInt(header.getValue(), -1); break;
          case "Content-Disposition" :

            if (fileName.length() == 0)
            {
              disposition = header.getValue();
              int index = disposition.indexOf("filename=");
              if (index > 0)
                assignSB(fileName, disposition.substring(index + 10, disposition.length() - 1));
            }
        }
      }

      if (fileName.length() == 0)
      {
        // extracts file name from URL

        String origFileNameStr = fileURL.substring(fileURL.lastIndexOf("/") + 1, fileURL.length());

        if (origFileNameStr.indexOf('?') >= 0)
          origFileNameStr = origFileNameStr.substring(0, origFileNameStr.indexOf('?'));

        if (origFileNameStr.length() == 0)
        {
          if (assumeIsImage)
            origFileNameStr = "image" + ZoteroWrapper.generateWriteToken();
          else
            origFileNameStr = "file" + ZoteroWrapper.generateWriteToken();
        }

        String ext = FilenameUtils.getExtension(origFileNameStr);

        if (ext.length() == 0)
        {
          if (contentType.length() > 0)
          {
            MimeType mimeType;
            try
            {
              mimeType = App.tika.getMimeRepository().forName(contentType);
              ext = mimeType.getExtension();
            }
            catch (MimeTypeException e) { noOp(); }

            if (ext.length() > 0)
              origFileNameStr = FilenameUtils.getBaseName(origFileNameStr) + FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
          }
        }

        if (assumeIsImage && (ext.length() == 0))
          origFileNameStr = FilenameUtils.getBaseName(origFileNameStr) + FilenameUtils.EXTENSION_SEPARATOR_STR + "jpg";

        assignSB(fileName, origFileNameStr);
      }

      if (saveToBuffer)
      {
        try (Buffer buffer = new Buffer(entity.getContent()))
        {
          runInFXThread(() -> successHndlr.handle(buffer));
        }
        catch (Exception e)
        {
          if (httpClient.wasCancelledByUser())
            runInFXThread(() -> failHndlr.handle(new TerminateTaskException()));
          else
            runInFXThread(() -> failHndlr.handle(e));

          return false;
        }
      }
      else
      {
        if (fileNameStr.length() == 0)
          saveFilePath = dirPath.resolve(new FilePath(fileName.toString()));
        else
          saveFilePath = dirPath.resolve(new FilePath(fileNameStr));

        // opens input stream from the HTTP connection
        // opens an output stream to save into file

        try (InputStream inputStream = entity.getContent();
             FileOutputStream outputStream = new FileOutputStream(saveFilePath.toFile()))
        {
          int bytesRead = -1;
          byte[] byteBuffer = new byte[BUFFER_SIZE];

          while ((bytesRead = inputStream.read(byteBuffer)) != -1)
          {
            outputStream.write(byteBuffer, 0, bytesRead);
          }

          runInFXThread(() -> successHndlr.handle(null));
        }
        catch (Exception e)
        {
          runInFXThread(() -> failHndlr.handle(e));
          return false;
        }
      }

      return true;
    };

    HttpUriRequest request;

    try
    {
      request =  RequestBuilder.get()
          .setUri(fileURL)
          .setHeader("User-Agent", "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:63.0) Gecko/20100101 Firefox/63.0")
          .build();
    }
    catch (Exception e)
    {
      runInFXThread(() -> failHndlr.handle(e));
      return;
    }

    httpClient.doRequest(request, responseHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
