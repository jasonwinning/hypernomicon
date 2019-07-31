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

package org.hypernomicon.view.settings;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.util.DesktopApi;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.dialogs.HyperDlg;
import org.hypernomicon.view.settings.SettingsDlgCtrlr;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.settings.LaunchCommandsDlgCtrlr.LaunchCommandTypeEnum.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.util.StringConverter;

public class LaunchCommandsDlgCtrlr extends HyperDlg
{
  @FXML private TextArea taCommands;
  @FXML private ListView<Preset> lvPresets;
  @FXML private ComboBox<LaunchCommandTypeEnum> cbCommandType;
  @FXML private TextField tfPath;
  @FXML private Button btnBrowse, btnInvoke, btnAppPathVar, btnFilePathVar, btnPageNumVar;

//---------------------------------------------------------------------------

  public static enum LaunchCommandTypeEnum
  {
    appleScript("appleScript", "AppleScript"),
    opSysCmdAndArgs("opSysCmdAndArgs", "Operating system command and arguments");

    private String prefVal, name;

    private LaunchCommandTypeEnum(String prefVal, String name)
    {
      this.prefVal = prefVal;
      this.name = name;
    }

    public static LaunchCommandTypeEnum getByPrefVal(String prefVal)
    {
      return findFirst(EnumSet.allOf(LaunchCommandTypeEnum.class),
                       typeEnum -> typeEnum.prefVal.equals(prefVal));
    }
  }

//---------------------------------------------------------------------------

  public static enum OperatingSystemEnum
  {
    windows, mac, linux, other;

    public static OperatingSystemEnum determine()
    {
      if (SystemUtils.IS_OS_WINDOWS) return windows;
      if (SystemUtils.IS_OS_MAC    ) return mac;
      if (SystemUtils.IS_OS_LINUX  ) return linux;

      return other;
    }
  }

//---------------------------------------------------------------------------

  private static class Preset
  {
    public Preset(OperatingSystemEnum opSys, String name, LaunchCommandTypeEnum commandType, String commandsPrefKey, String commands)
    {
      this.opSys = opSys;
      this.name = name;
      this.commandType = commandType;
      this.commandsPrefKey = commandsPrefKey;
      this.commands = commands;
    }

    final private OperatingSystemEnum opSys;
    final private LaunchCommandTypeEnum commandType;
    final private String commandsPrefKey, commands, name;

    public boolean isCompatible() { return opSys == OperatingSystemEnum.determine(); }
  }

//---------------------------------------------------------------------------

  public static final String appPathVar = "::::AppPath", filePathVar = "::::FilePath", pageNumVar = "::::PageNum";

  private String appPrefKey, commandsPrefKey, commandTypePrefKey;

  public static final StringConverter<LaunchCommandTypeEnum> typeStrConv = new StringConverter<>()
  {
    @Override public String toString(LaunchCommandTypeEnum commandTypeEnum)
    { return commandTypeEnum == null ? "" : commandTypeEnum.name; }

    @Override public LaunchCommandTypeEnum fromString(String string)
    { return findFirst(EnumSet.allOf(LaunchCommandTypeEnum.class), typeEnum -> typeEnum.name.equals(string)); }
  };

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static LaunchCommandsDlgCtrlr create(String title, String appPrefKey, String commandsPrefKey, String commandTypePrefKey)
  {
    LaunchCommandsDlgCtrlr lcdc = HyperDlg.createUsingFullPath("view/settings/LaunchCommandsDlg.fxml", title, true);
    lcdc.init(appPrefKey, commandsPrefKey, commandTypePrefKey);
    return lcdc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(String appPrefKey, String commandsPrefKey, String commandTypePrefKey)
  {
    this.appPrefKey = appPrefKey;
    this.commandsPrefKey = commandsPrefKey;
    this.commandTypePrefKey = commandTypePrefKey;

    onShown = () -> disableCache(taCommands);

    btnBrowse.setOnAction(event -> SettingsDlgCtrlr.browseClick(dialogStage, tfPath));

    taCommands.setText(appPrefs.get(commandsPrefKey, ""));
    tfPath.setText(appPrefs.get(appPrefKey, ""));

    btnAppPathVar .setOnAction(event -> taCommands.insertText(taCommands.getCaretPosition(), appPathVar ));
    btnFilePathVar.setOnAction(event -> taCommands.insertText(taCommands.getCaretPosition(), filePathVar));
    btnPageNumVar .setOnAction(event -> taCommands.insertText(taCommands.getCaretPosition(), pageNumVar ));

    btnAppPathVar .setText(appPathVar);
    btnFilePathVar.setText(filePathVar);
    btnPageNumVar .setText(pageNumVar);

    lvPresets.setCellFactory(listView -> new ListCell<>()
    {
      @Override protected void updateItem(Preset item, boolean empty)
      {
        super.updateItem(item, empty);
        setText(empty || (item == null) ? null : item.name);
      }
    });

    btnInvoke.disableProperty().bind(lvPresets.getSelectionModel().selectedItemProperty().isNull());

    btnInvoke.setOnAction(event ->
    {
      if (taCommands.getText().length() > 0)
        if (confirmDialog("Replace current settings with selected preset?") == false)
          return;

      Preset preset = lvPresets.getSelectionModel().getSelectedItem();
      taCommands.setText(preset.commands);
      cbCommandType.getSelectionModel().select(preset.commandType);
    });

    String typePrefVal = appPrefs.get(commandTypePrefKey, opSysCmdAndArgs.prefVal);

    cbCommandType.setItems(FXCollections.observableArrayList(EnumSet.allOf(LaunchCommandTypeEnum.class)));
    cbCommandType.setConverter(typeStrConv);

    cbCommandType.getSelectionModel().select(LaunchCommandTypeEnum.getByPrefVal(typePrefVal));

    presets.forEach(preset ->
    {
      if (preset.isCompatible() && preset.commandsPrefKey.equals(commandsPrefKey))
        lvPresets.getItems().add(preset);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    appPrefs.put(appPrefKey, tfPath.getText());
    appPrefs.put(commandsPrefKey, taCommands.getText());
    appPrefs.put(commandTypePrefKey, nullSwitch(cbCommandType.getSelectionModel().getSelectedItem(), "", typeEnum -> typeEnum.prefVal));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launch(String appPath, FilePath filePath, String commandsPrefKey, String commandTypePrefKey, int pageNum)
  {
    String[] argz;
    String commands = appPrefs.get(commandsPrefKey, "");
    LaunchCommandTypeEnum commandType = getByPrefVal(appPrefs.get(commandTypePrefKey, ""));

    if ((commandType != null) && (commands.length() > 0))
    {
      if ((commandType == appleScript) && SystemUtils.IS_OS_MAC)
      {
        argz = new String[] { "osascript",
                              "-e",
                              resolve(commands, appPath, filePath, pageNum) };
        try
        {
          Runtime.getRuntime().exec(argz);
        }
        catch (IOException e)
        {
          messageDialog("An error occurred while trying to start application: " + e.getMessage(), mtError);
        }

        return;
      }

      if (commandType == opSysCmdAndArgs)
      {
        ArrayList<String> list = convertMultiLineStrToStrList(resolve(commands, appPath, filePath, pageNum), false);

        if (list.size() > 1)
        {
          DesktopApi.exec(true, false, new StringBuilder(), list);
          return;
        }
      }
    }

    DesktopApi.exec(true, false, new StringBuilder(), appPath, filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String resolve(String commands, String appPath, FilePath filePath, int pageNum)
  {
    while (commands.contains(appPathVar))
      commands = commands.replace(appPathVar, appPath);

    while (commands.contains(filePathVar))
      commands = commands.replace(filePathVar, filePath.toString());

    String pageNumStr = pageNum >= 1 ? String.valueOf(pageNum) : "";

    while (commands.contains(pageNumVar))
      commands = commands.replace(pageNumVar, pageNumStr);

    return commands;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<Preset> presets = List.of(

    new Preset(OperatingSystemEnum.windows, "Adobe Acrobat", opSysCmdAndArgs, PREF_KEY_PDF_READER_COMMANDS,
               appPathVar + "\n" +
               "/A\n" +
               "page=" + pageNumVar + "\n" +
               filePathVar),

    new Preset(OperatingSystemEnum.mac, "Preview (macOS)", appleScript, PREF_KEY_PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\"\n" +
               "end tell\n" +
               "tell application \"System Events\"\n" +
               "  keystroke \"g\" using {option down, command down}\n" +
               "  keystroke " + pageNumVar + "\n" +
               "  keystroke return\n" +
               "end tell"),

    new Preset(OperatingSystemEnum.mac, "Adobe Acrobat", appleScript, PREF_KEY_PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\" options \"page=" + pageNumVar + "\"\n" +
               "end tell"));

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}