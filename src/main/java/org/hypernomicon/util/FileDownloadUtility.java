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

import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.*;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.methods.RequestBuilder;

import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.mainText.MainTextWrapper;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * FileDownloadUtility provides methods for downloading files from a given URL. It supports
 * downloading files to the file system or into memory buffers. The utility handles HTTP
 * connections, responses, and saving the file data.
 *
 * <p>Example usage:<pre>
 * // Download to file
 * FileDownloadUtility.downloadToFile(fileURL, dirPath, fileNameStr, fileName, assumeIsImage, httpClient, successHandler, failHandler);
 *
 * // Download to buffer
 * FileDownloadUtility.downloadToBuffer(fileURL, fileName, assumeIsImage, httpClient, successHandler, failHandler);</pre>
 * <p>This class is not intended to be instantiated.
 */
public final class FileDownloadUtility
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int BUFFER_SIZE = 4096;

//---------------------------------------------------------------------------

  private FileDownloadUtility() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class Buffer extends InputStream
  {

  //---------------------------------------------------------------------------

    private final List<byte[]> buffers = new ArrayList<>();
    private final List<Integer> lengths = new ArrayList<>();
    private int curBufferNdx = 0, curPosition = 0;

  //---------------------------------------------------------------------------

    private Buffer(InputStream inputStream) throws IOException
    {
      int bytesRead;
      byte[] buffer = new byte[BUFFER_SIZE];

      while ((bytesRead = inputStream.read(buffer)) != -1)
      {
        buffers.add(buffer);
        lengths.add(bytesRead);
        buffer = new byte[BUFFER_SIZE];
      }
    }

  //---------------------------------------------------------------------------

    public void saveToFile(FilePath saveFilePath) throws IOException
    {
      try (OutputStream os = Files.newOutputStream(saveFilePath.toPath()))
      {
        for (int bufferNdx = 0; bufferNdx < buffers.size(); bufferNdx++)
          os.write(buffers.get(bufferNdx), 0, lengths.get(bufferNdx));
      }
    }

  //---------------------------------------------------------------------------

    @Override public int read() throws IOException
    {
      if (curPosition >= lengths.get(curBufferNdx))
      {
        curPosition = 0;
        curBufferNdx++;
      }

      if (curBufferNdx >= buffers.size())
        throw new IOException("End of buffer");

      byte val = buffers.get(curBufferNdx)[curPosition++];

      return val + 128;
    }

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Downloads a file from the specified URL and saves it to the given directory with the specified file name.
   *
   * @param fileURL       the URL of the file to be downloaded
   * @param dirPath       the directory path where the file should be saved
   * @param fileNameStr   the name of the file to be saved (if empty, it will try to use the file name from the server)
   * @param fileName      output-only parameter; a StringBuilder to hold the resulting file name
   * @param assumeIsImage a flag indicating whether to assume the file is an image if no file extension is found
   * @param httpClient    the AsyncHttpClient to handle HTTP requests
   * @param successHndlr  a consumer to handle the Buffer upon successful download
   * @param failHndlr     a consumer to handle exceptions in case of failure
   */
  public static void downloadToFile(String fileURL, FilePath dirPath, String fileNameStr, StringBuilder fileName, boolean assumeIsImage,
                                    AsyncHttpClient httpClient, Consumer<Buffer> successHndlr, Consumer<Exception> failHndlr)
  {
    downloadFile(fileURL, dirPath, fileNameStr, false, fileName, assumeIsImage, httpClient, successHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Downloads a file from the specified URL and stores it in an in-memory buffer.
   * The {@code fileName} parameter will be set to the server-specified file name if it's available.
   * The {@code fileName} parameter is always cleared before the download begins.
   *
   * @param fileURL       the URL of the file to be downloaded
   * @param fileName      a StringBuilder to hold the resulting file name; it will be cleared before use
   * @param assumeIsImage a flag indicating whether to assume the file is an image if no file extension is found
   * @param httpClient    the AsyncHttpClient to handle HTTP requests
   * @param successHndlr  a consumer to handle the Buffer upon successful download
   * @param failHndlr     a consumer to handle exceptions in case of failure
   */
  public static void downloadToBuffer(String fileURL, StringBuilder fileName, boolean assumeIsImage,
                                      AsyncHttpClient httpClient, Consumer<Buffer> successHndlr, Consumer<Exception> failHndlr)
  {
    downloadFile(fileURL, null, "", true, fileName, assumeIsImage, httpClient, successHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void downloadFile(String fileURL, FilePath dirPath, String fileNameStr, boolean saveToBuffer, StringBuilder fileName,
                                   boolean assumeIsImage, AsyncHttpClient httpClient, Consumer<Buffer> successHndlr, Consumer<Exception> failHndlr)
  {
    assignSB(fileName, "");

    ResponseHandler<Boolean> responseHndlr = response -> handleResponse(response, fileURL, dirPath, fileNameStr, saveToBuffer, fileName,
                                                                        assumeIsImage, httpClient, successHndlr, failHndlr);

    HttpUriRequest request = RequestBuilder.get()
      .setUri(fileURL)
      .setHeader("User-Agent", MainTextWrapper.getUserAgent())
      .build();

    httpClient.doRequest(request, responseHndlr, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean handleResponse(HttpResponse response, String fileURL, FilePath dirPath, String fileNameStr, boolean saveToBuffer,
                                        StringBuilder fileName, boolean assumeIsImage, AsyncHttpClient httpClient,
                                        Consumer<Buffer> successHndlr, Consumer<Exception> failHndlr)
  {
    MutableInt contentLength = new MutableInt(-1);

    int statusCode = response.getStatusLine().getStatusCode();
    String reasonPhrase = response.getStatusLine().getReasonPhrase(),
           contentType = "";

    if (statusCode >= 400)
    {
      runInFXThread(() -> failHndlr.accept(new HttpResponseException(statusCode, reasonPhrase)));
      return false;
    }

    HttpEntity entity = response.getEntity();

    for (Header header : response.getAllHeaders())
    {
      switch (HttpHeader.get(header))
      {
        case Content_Type : contentType = header.getValue(); break;
        case Content_Length : contentLength.setValue(parseInt(header.getValue(), -1)); break;
        case Content_Disposition :

          if (fileName.isEmpty())
          {
            String disposition = header.getValue();
            int index = disposition.indexOf("filename=");
            if (index > 0)
              assignSB(fileName, disposition.substring(index + 10, disposition.length() - 1));
          }

          break;

        default : break;
      }
    }

    if (fileName.isEmpty())
    {
      // extracts file name from URL

      String origFileNameStr = fileURL.substring(fileURL.lastIndexOf('/') + 1);

      if (origFileNameStr.indexOf('?') >= 0)
        origFileNameStr = origFileNameStr.substring(0, origFileNameStr.indexOf('?'));

      if (origFileNameStr.indexOf('&') >= 0)
        origFileNameStr = origFileNameStr.substring(0, origFileNameStr.indexOf('&'));

      if (origFileNameStr.indexOf(':') >= 0)
        origFileNameStr = origFileNameStr.endsWith(":") ? "" : origFileNameStr.substring(origFileNameStr.lastIndexOf(':') + 1);

      if (origFileNameStr.isEmpty())
        origFileNameStr = (assumeIsImage ? "image" : "file") + randomHexStr(32);

      String ext = FilenameUtils.getExtension(origFileNameStr);

      if (ext.isEmpty() && (contentType.length() > 0))
      {
        ext = getContentTypeExtension(contentType);

        if (ext.length() > 0)
          origFileNameStr = FilenameUtils.getBaseName(origFileNameStr) + FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
      }

      if (assumeIsImage && ext.isEmpty())
        origFileNameStr = FilenameUtils.getBaseName(origFileNameStr) + FilenameUtils.EXTENSION_SEPARATOR_STR + "jpg";

      assignSB(fileName, origFileNameStr);
    }

    if (saveToBuffer)
    {
      try (Buffer buffer = new Buffer(entity.getContent()))
      {
        runInFXThread(() -> successHndlr.accept(buffer));
      }
      catch (IOException e)
      {
        runInFXThread(() -> failHndlr.accept(httpClient.wasCancelledByUser() ? new CancelledTaskException() : e));

        return false;
      }
    }
    else
    {
      FilePath saveFilePath = dirPath.resolve(fileNameStr.isEmpty() ? fileName.toString() : fileNameStr);

      // opens input stream from the HTTP connection
      // opens an output stream to save into file

      try (InputStream inputStream = entity.getContent();
           OutputStream outputStream = Files.newOutputStream(saveFilePath.toPath()))
      {
        int bytesRead;
        byte[] byteBuffer = new byte[BUFFER_SIZE];

        while ((bytesRead = inputStream.read(byteBuffer)) != -1)
          outputStream.write(byteBuffer, 0, bytesRead);

        runInFXThread(() -> successHndlr.accept(null));
      }
      catch (IOException e)
      {
        runInFXThread(() -> failHndlr.accept(e));
        return false;
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
