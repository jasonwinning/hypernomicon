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

package org.hypernomicon.previewWindow;

import static org.hypernomicon.App.app;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;

import org.jodconverter.core.office.OfficeException;
import org.jodconverter.core.office.OfficeUtils;
import org.jodconverter.local.LocalConverter;
import org.jodconverter.local.office.LocalOfficeManager;

//---------------------------------------------------------------------------

public final class OfficePreviewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record OfficePreviewInfo(PreviewWrapper previewWrapper, PDFJSWrapper jsWrapper, FilePath filePath, int pageNum, boolean convertToHtml, String officePath) { }

  private static OfficePreviewThread bkgThread;

  private static Map<PDFJSWrapper, Boolean> wrapperStopped = new ConcurrentHashMap<>();

  private static volatile OfficePreviewInfo lastInfo, nextInfo;

  private OfficePreviewer() { throw new UnsupportedOperationException(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void preview(String mimetypeStr, FilePath filePath, int pageNum, PDFJSWrapper jsWrapper, PreviewWrapper previewWrapper)
  {
    if (previewWrapper != null)
      assert (jsWrapper == previewWrapper.getJSWrapper());

    String officePath = app.prefs.get(PREF_KEY_OFFICE_PATH, "");

    if (officePath.isBlank())
    {
      if (lastInfo != null)
        wrapperStopped.put(jsWrapper, true);

      jsWrapper.setNoOfficeInstallation();
      return;
    }

    if (bkgThread == null)
      (bkgThread = new OfficePreviewThread()).start();

    jsWrapper.setGenerating(filePath);

    boolean convertToHtml = mimetypeStr.contains("spreadsheetml.sheet")      ||  // xlsx (Microsoft Excel XML)
                            mimetypeStr.contains("ms-excel")                 ||  // xls  (Microsoft Excel)
                            "text/csv".equalsIgnoreCase(mimetypeStr)         ||  // csv  (Comma-separated values)
                            mimetypeStr.contains("tab-separated-values")     ||  // tsv  (Tab-separated values)
                            mimetypeStr.contains("opendocument.spreadsheet") ||  // ods  (OpenDocument spreadsheet), ots (OpenDocument spreadsheet template)
                            mimetypeStr.contains("sun.xml.calc");                // sxc  (OpenOffice.org 1.0 spreadsheet)

    if ((lastInfo != nextInfo) && (nextInfo != null) && (nextInfo.jsWrapper != jsWrapper) && (nextInfo.previewWrapper != null))
      nextInfo.previewWrapper.setNeedsRefresh(nextInfo.filePath);

    nextInfo = new OfficePreviewInfo(previewWrapper, jsWrapper, filePath, pageNum, convertToHtml, officePath);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static synchronized void stopPreview()
  {
    wrapperStopped.keySet().forEach(jsWrapper -> wrapperStopped.put(jsWrapper, true));

    nextInfo = null;
  }

  public static synchronized void stopPreview(PDFJSWrapper jsWrapper)
  {
    wrapperStopped.put(jsWrapper, true);

    if ((nextInfo != null) && (nextInfo.jsWrapper == jsWrapper))
      nextInfo = null;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private static final class OfficePreviewThread extends HyperThread
  {
    private static volatile LocalOfficeManager officeManager;
    private static volatile LocalConverter officeConverter;

    private static volatile boolean shutDown;

//---------------------------------------------------------------------------

    private OfficePreviewThread()
    {
      super("OfficePreview");

      setDaemon(false);
    }

//---------------------------------------------------------------------------

    @Override public void run()
    {
      Map<PDFJSWrapper, FilePath> wrapperToTempDir = new HashMap<>();
      File tempPath = null;

      while (shutDown == false)
      {
        while ((nextInfo == null) && (shutDown == false))
          sleepForMillis(100);

        if (shutDown)
          break;

        try
        {
          FilePath tempDir = wrapperToTempDir.get(nextInfo.jsWrapper);
          if (tempDir != null)
            FileUtils.deleteDirectory(tempDir.toFile());

          tempDir = tempOfficePreviewFolder(false, false).resolve("preview" + randomAlphanumericStr(8));
          tempPath = tempDir.resolve("preview" + randomAlphanumericStr(8) + '.' + (nextInfo.convertToHtml ? "html" : "pdf")).toFile();

          wrapperToTempDir.put(nextInfo.jsWrapper, tempDir);
        }
        catch (IOException e)
        {
          nextInfo.jsWrapper.setUnable(nextInfo.filePath);
          stopPreview(nextInfo.jsWrapper);
          wrapperToTempDir.remove(nextInfo.jsWrapper);
          continue;
        }

        if (updateOfficeConverter(nextInfo.officePath) == false)
        {
          nextInfo.jsWrapper.setUnable(nextInfo.filePath);
          stopPreview(nextInfo.jsWrapper);
          continue;
        }

        lastInfo = nextInfo;
        wrapperStopped.put(nextInfo.jsWrapper, false);

        try
        {
          officeConverter.convert(lastInfo.filePath.toFile()).to(tempPath).execute();
        }
        catch (OfficeException e)
        {
          if ((nextInfo == lastInfo) || ((nextInfo != null) && (lastInfo.jsWrapper != nextInfo.jsWrapper)))
            lastInfo.jsWrapper.setUnable(lastInfo.filePath);

          if (nextInfo == lastInfo)
            nextInfo = null;

          continue;
        }

        if (shutDown || Boolean.TRUE.equals(wrapperStopped.get(lastInfo.jsWrapper)) || ((nextInfo != null) && (lastInfo != nextInfo) && (lastInfo.jsWrapper == nextInfo.jsWrapper)))
          continue;

        if (lastInfo.convertToHtml)
          try
          {
            lastInfo.jsWrapper.loadFile(new FilePath(tempPath), false);
          }
          catch (IOException e)
          {
            lastInfo.jsWrapper.setUnable(lastInfo.filePath);
          }
        else
          lastInfo.jsWrapper.loadPdf(new FilePath(tempPath), lastInfo.pageNum);

        if (nextInfo == lastInfo)
          nextInfo = null;
      }

      if (officeConverter != null)
        OfficeUtils.stopQuietly(officeManager);
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    /**
     * Make sure an officeManager exists corresponding to the current office installation path; if not,
     * stop the existing officeManager if there is one and create a new one for the current installation path
     * @param officePath The currently configured office installation path
     * @return True if able to do conversions; false otherwise
     */
    private static boolean updateOfficeConverter(String officePath)
    {
      if (officePath.isBlank())
        return false;

      if ((lastInfo == null) || (lastInfo.officePath.equals(officePath) == false))
      {
        if (officeConverter != null)
        {
          OfficeUtils.stopQuietly(officeManager);
          officeConverter = null;
        }

        try
        {
          tempOfficePreviewFolder(true, true);

          List<Integer> ports = new ArrayList<>();
          DesktopUtil.findAvailablePorts(1, ports);

          if (SystemUtils.IS_OS_MAC && officePath.toLowerCase().endsWith(".app"))  // Allow user to just select the app file
            officePath = officePath + "/Contents";

          officeManager = LocalOfficeManager.builder().portNumbers(ports.get(0))
                                                      .officeHome(officePath)
                                                      .build();

          officeManager.start();

          officeConverter = LocalConverter.make(officeManager);
        }
        catch (OfficeException | IllegalStateException | IOException e)
        {
          OfficeUtils.stopQuietly(officeManager);

          officeConverter = null;
          return false;
        }
      }

      return true;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private static final String tempOfficePreviewFolderName = "hnTempOfficePreview";

    private static FilePath tempOfficePreviewFolder(boolean create, boolean clear) throws IOException
    {
      FilePath filePath = DesktopUtil.tempDir().resolve(tempOfficePreviewFolderName);

      if ((create == false) && (clear == false))
        return filePath;

      if (filePath.exists())
      {
        if (clear)
          FileUtils.cleanDirectory(filePath.toFile());
      }
      else
      {
        if (create)
          filePath.createDirectory();
      }

      return filePath;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private static void cleanup()
    {
      shutDown = true;

      OfficeUtils.stopQuietly(officeManager);

      officeConverter = null;
    }

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void cleanup()
  {
    stopPreview();

    new HyperThread(OfficePreviewThread::cleanup, "OfficePreviewCleanup").start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
