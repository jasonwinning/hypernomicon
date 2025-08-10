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

package org.hypernomicon.testTools.xmlDiff;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.testTools.xmlDiff.XmlDiffApp.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;

import java.util.ArrayList;
import java.util.Arrays;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.Region;
import javafx.stage.*;

//---------------------------------------------------------------------------

public final class XmlDiffCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnBackup, btnTest, btnOldProcessed, btnNewProcessed, btnBackupLaunch, btnTestLaunch, btnOldProcessedLaunch, btnNewProcessedLaunch,
                       btnProcess, btnBackupToTest, btnTestToBackup, btnExeBrowse1, btnExeBrowse2, btnExeBrowse3, btnExeBrowse4;
  @FXML private RadioButton rbExe1, rbExe2, rbExe3, rbExe4;
  @FXML private TextField tfBackup, tfTest, tfOldProcessed, tfNewProcessed, tfExe1, tfExe2, tfExe3, tfExe4;
  @FXML private ToggleGroup tgExe;

  private final Stage stage;

  private static final List<String> fileNames = List.of("Arguments.xml", "Debates.xml", "Files.xml", "Hubs.xml", "Institutions.xml", "Investigations.xml",
                                                        "Notes.xml", "Other.xml", "People.xml", "Positions.xml", "Terms.xml", "Works.xml");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void create(Stage stage) throws IOException
  {
    noOp(new XmlDiffCtrlr(stage));
  }

  private XmlDiffCtrlr(Stage stage) throws IOException
  {
    synchronized(XmlDiffCtrlr.class)
    {
      if (xmlDiffCtrlr != null)
        throw new UnsupportedOperationException("XmlDiffCtrlr can only be instantiated once.");

      xmlDiffCtrlr = this;
    }

    FXMLLoader loader = new FXMLLoader(XmlDiffApp.class.getResource("XmlDiff.fxml"), null, null, klass -> this);
    Region rootNode = loader.load();

    this.stage = stage;
    Scene scene = new Scene(rootNode);

    stage.setScene(scene);

    stage.show();

    btnBackup      .setOnAction(event -> btnBrowseClick("Choose folder with backup XML files"      , tfBackup));
    btnTest        .setOnAction(event -> btnBrowseClick("Choose folder with test XML files"        , tfTest));
    btnOldProcessed.setOnAction(event -> btnBrowseClick("Choose output folder for backup XML files", tfOldProcessed));
    btnNewProcessed.setOnAction(event -> btnBrowseClick("Choose output folder for test XML files"  , tfNewProcessed));

    btnBackupLaunch      .setOnAction(event -> btnLaunchClick(tfBackup));
    btnTestLaunch        .setOnAction(event -> btnLaunchClick(tfTest));
    btnOldProcessedLaunch.setOnAction(event -> btnLaunchClick(tfOldProcessed));
    btnNewProcessedLaunch.setOnAction(event -> btnLaunchClick(tfNewProcessed));

    btnExeBrowse1.setOnAction(event -> btnExeBrowseClick(1));
    btnExeBrowse2.setOnAction(event -> btnExeBrowseClick(2));
    btnExeBrowse3.setOnAction(event -> btnExeBrowseClick(3));
    btnExeBrowse4.setOnAction(event -> btnExeBrowseClick(4));

    btnBackupToTest.setOnAction(event -> moveFiles(tfBackup, tfTest));
    btnTestToBackup.setOnAction(event -> moveFiles(tfTest, tfBackup));

    btnProcess     .setOnAction(event -> process());

    loadPaths();
  }

//---------------------------------------------------------------------------

  private static void btnLaunchClick(TextField tf) { launchFile(new FilePath(tf.getText())); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void moveFiles(TextField tfSrc, TextField tfDest)
  {
    FilePath srcFolderPath  = new FilePath(tfSrc .getText()),
             destFolderPath = new FilePath(tfDest.getText());

    try
    {
      FileUtils.cleanDirectory(destFolderPath.toFile());

      FilePathSet srcSet = new FilePathSet();

      srcFolderPath.addDirContentsToSet(srcSet);

      for (FilePath srcFilePath : srcSet)
      {
        if ((srcFilePath.isDirectory() == false) && srcFilePath.getDirOnly().equals(srcFolderPath))
        {
          FilePath destFilePath = destFolderPath.resolve(srcFolderPath.relativize(srcFilePath));
          srcFilePath.copyTo(destFilePath, false);
        }
      }
    }
    catch (HyperDataException | IOException e)
    {
      errorPopup(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBrowseClick(String title, TextField tf)
  {
    DirectoryChooser dirChooser = new DirectoryChooser();
    dirChooser.setTitle(title);

    if (tf.getText().isEmpty() == false)
      dirChooser.setInitialDirectory(new File(tf.getText()));

    File file = dirChooser.showDialog(stage);

    if (file != null)
      tf.setText(file.getAbsolutePath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void savePaths()
  {
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_BACKUP_PATH     , tfBackup      .getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_TEST_PATH       , tfTest        .getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_BACKUP_TEMP_PATH, tfOldProcessed.getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_TEST_TEMP_PATH  , tfNewProcessed.getText());

    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_EXECUTABLE_1, tfExe1.getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_EXECUTABLE_2, tfExe2.getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_EXECUTABLE_3, tfExe3.getText());
    xmlDiffApp.prefs.put(PrefKey.XML_DIFF_EXECUTABLE_4, tfExe4.getText());

    xmlDiffApp.prefs.putInt(PrefKey.XML_DIFF_SELECTED_EXE_NUM, selectedExeNum());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadPaths()
  {
    tfBackup      .setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_BACKUP_PATH     , ""));
    tfTest        .setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_TEST_PATH       , ""));
    tfOldProcessed.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_BACKUP_TEMP_PATH, ""));
    tfNewProcessed.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_TEST_TEMP_PATH  , ""));

    tfExe1.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_EXECUTABLE_1, ""));
    tfExe2.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_EXECUTABLE_2, ""));
    tfExe3.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_EXECUTABLE_3, ""));
    tfExe4.setText(xmlDiffApp.prefs.get(PrefKey.XML_DIFF_EXECUTABLE_4, ""));

    tgExe.selectToggle(getRadioButton(xmlDiffApp.prefs.getInt(PrefKey.XML_DIFF_SELECTED_EXE_NUM, 1)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnExeBrowseClick(int num)
  {
    TextField tf = getExeField(num);

    FilePath startPath = new FilePath(tf.getText()).getDirOnly();

    if (FilePath.isEmpty(startPath))
      startPath = new FilePath(userWorkingDir());

    FileChooser fileChooser = new FileChooser();

    if (SystemUtils.IS_OS_WINDOWS)
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Executable files (*.exe)", "*.exe"));
    else if (SystemUtils.IS_OS_MAC)
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("App files (*.app)", "*.app"));

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(startPath.toFile());

    fileChooser.setTitle("Select diff tool executable");

    File file = fileChooser.showOpenDialog(stage);

    if (file != null)
      tf.setText(file.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int selectedExeNum()
  {
    Toggle toggle = tgExe.getSelectedToggle();

    if (toggle == rbExe1) return 1;
    if (toggle == rbExe2) return 2;
    if (toggle == rbExe3) return 3;
    if (toggle == rbExe4) return 4;

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TextField getExeField(int num)
  {
    return switch (num)
    {
      case 1 -> tfExe1;
      case 2 -> tfExe2;
      case 3 -> tfExe3;
      case 4 -> tfExe4;

      default -> throw new IllegalArgumentException("Unexpected value: " + num);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RadioButton getRadioButton(int num)
  {
    return switch (num)
    {
      case 1 -> rbExe1;
      case 2 -> rbExe2;
      case 3 -> rbExe3;
      case 4 -> rbExe4;

      default -> throw new IllegalArgumentException("Unexpected value: " + num);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void process()
  {
    try
    {
      processForFolder(tfBackup.getText(), tfOldProcessed.getText());
      processForFolder(tfTest  .getText(), tfNewProcessed.getText());

      ProcessBuilder pb = new ProcessBuilder(List.of(getExeField(selectedExeNum()).getText(), tfOldProcessed.getText(), tfNewProcessed.getText()));

      Process proc = pb.start();

      proc.onExit().thenRun(() ->
      {
        if (proc.exitValue() != 0)
        {
          try (InputStream is = proc.getErrorStream())
          {
            StringBuilder errorSB = new StringBuilder();
            assignSB(errorSB, IOUtils.toString(is, StandardCharsets.UTF_8));
            errorPopup(errorSB.toString());
          }
          catch (IOException e)
          {
            errorPopup(e);
          }
        }
      });
    }
    catch (IOException e)
    {
      errorPopup(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String ESCAPED_GT = "&gt;",
                              ESCAPED_LT = "&lt;";

  private static final int ESCAPED_GT_LEN = ESCAPED_GT.length();

//---------------------------------------------------------------------------

  private void processForFolder(String srcPathStr, String destPathStr) throws IOException
  {
    for (String fileName : fileNames)
    {
      // 1. Read original lines
      List<String> originalLines = FileUtils.readLines(Path.of(srcPathStr, fileName).toFile(), Charset.defaultCharset());

      // 2. First pass: split at each "&gt;" (after) and "&lt;" (before),
      //    stripping trailing/leading spaces on each fragment
      List<String> intermediateLines = new ArrayList<>();

      for (String line : originalLines)
      {
        String nextLine = line.stripTrailing();
        boolean splitHappened = false;

        while (true)
        {
          int posGT = nextLine.indexOf(ESCAPED_GT),
              posLT = nextLine.isEmpty() ? -1 : nextLine.indexOf(ESCAPED_LT, 1);

          // no more tags; bail out
          if ((posGT == -1) && (posLT == -1))
            break;

          int splitPos;
          if ((posGT != -1) && ((posLT == -1) || (posGT < posLT)))
          {
            // split *after* "&gt;"
            splitPos = posGT + ESCAPED_GT_LEN;
          }
          else
          {
            // split *before* "&lt;"
            splitPos = posLT;
          }

          // take the chunk up to splitPos
          String newLine = nextLine.substring(0, splitPos).strip();
          if (newLine.isEmpty() == false)
            intermediateLines.add(newLine);

          // continue with the rest
          nextLine = nextLine.substring(splitPos).stripLeading();

          splitHappened = true;
        }

        // whatever remains, add as its own line
        if ((splitHappened == false) || (nextLine.isEmpty() == false))
          intermediateLines.add(splitHappened ? nextLine.strip() : nextLine.stripTrailing());
      }

      // 3. Rejoin for any cross-line regex work
      String content = String.join("\n", intermediateLines);

      // 4. Normalize line breaks before &lt; when needed
      content = content.replaceAll("(?<=[^>;\\s])\\s*(?=&lt;)", "\n");

      // 5. Remove indent in lines that follow &gt;
      content = content.replaceAll("(?<=&gt;\\n)\\s+", "");

      // 6. Split back out into final lines
      List<String> finalLines = Arrays.asList(content.split("\n", -1));

      // 7. Write the result
      FileUtils.writeLines(Path.of(destPathStr, fileName).toFile(), finalLines);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
