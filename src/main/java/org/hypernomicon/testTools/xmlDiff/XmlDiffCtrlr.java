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

package org.hypernomicon.testTools.xmlDiff;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.testTools.xmlDiff.XmlDiffApp.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathSet;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import java.util.ArrayList;

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

  private static final Object LOCK = new Object();
  private static final List<String> fileNames = List.of("Arguments.xml", "Debates.xml", "Files.xml", "Hubs.xml", "Institutions.xml", "Investigations.xml",
                                                        "Notes.xml", "Other.xml", "People.xml", "Positions.xml", "Settings.xml", "Terms.xml", "Works.xml");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void create(Stage stage) throws IOException
  {
    noOp(new XmlDiffCtrlr(stage));
  }

  private XmlDiffCtrlr(Stage stage) throws IOException
  {
    synchronized(LOCK)
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

    if (IS_OS_WINDOWS)
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Executable files (*.exe)", "*.exe"));
    else if (IS_OS_MAC)
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
            errorPopup(IOUtils.toString(is, StandardCharsets.UTF_8));
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

  private static void processForFolder(String srcPathStr, String destPathStr) throws IOException
  {
    for (String fileName : fileNames)
    {
      List<String> originalLines = FileUtils.readLines(Path.of(srcPathStr, fileName).toFile(), StandardCharsets.UTF_8);

      FileUtils.writeLines(Path.of(destPathStr, fileName).toFile(), processLines(originalLines));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Put each escaped HTML tag on its own line so the external diff tool
  // can compare them individually rather than showing entire description
  // blocks as single-line changes.

  static List<String> processLines(Iterable<String> originalLines)
  {
    List<String> processedLines = new ArrayList<>();

    for (String line : originalLines)
    {
      String nextLine = line.stripTrailing();
      boolean splitHappened = false;

      while (true)
      {
        int posGT = nextLine.indexOf(ESCAPED_GT),
            posLT = nextLine.indexOf(ESCAPED_LT, 1);  // skip position 0 (already starts this line)

        if ((posGT == -1) && (posLT == -1))
          break;

        int splitPos;
        if ((posGT != -1) && ((posLT == -1) || (posGT < posLT)))
        {
          splitPos = posGT + ESCAPED_GT_LEN;   // split *after* "&gt;"
        }
        else
        {
          splitPos = posLT;                     // split *before* "&lt;"
        }

        String newLine = nextLine.substring(0, splitPos).stripTrailing();
        if (newLine.isEmpty() == false)
          processedLines.add(newLine);

        nextLine = nextLine.substring(splitPos).stripLeading();

        splitHappened = true;
      }

      if ((splitHappened == false) || (nextLine.isEmpty() == false))
        processedLines.add(nextLine.stripTrailing());
    }

    // Lines following an escaped closing tag (e.g. &lt;/html&gt;) may have
    // inherited indentation from the original XML; strip it so the diff is clean.

    for (int ndx = 1; ndx < processedLines.size(); ndx++)
      if (processedLines.get(ndx - 1).endsWith(ESCAPED_GT))
        processedLines.set(ndx, processedLines.get(ndx).stripLeading());

    return processedLines;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
