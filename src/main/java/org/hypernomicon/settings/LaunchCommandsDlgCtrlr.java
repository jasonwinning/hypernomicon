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

package org.hypernomicon.settings;

import org.apache.commons.lang3.SystemUtils;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.settings.LaunchCommandsDlgCtrlr.LaunchCommandTypeEnum.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.EnumSet;
import java.util.List;

import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public class LaunchCommandsDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnBrowse, btnInvoke, btnAppPathVar, btnFilePathVar, btnPageNumVar;
  @FXML private ComboBox<LaunchCommandTypeEnum> cbCommandType;
  @FXML private ListView<Preset> lvPresets;
  @FXML private TextArea taCommands;
  @FXML private TextField tfPath;

//---------------------------------------------------------------------------

  public enum LaunchCommandTypeEnum
  {
    appleScript("appleScript", "AppleScript"),
    opSysCmdAndArgs("opSysCmdAndArgs", "Operating system command and arguments");

    private final String prefVal, name;

    LaunchCommandTypeEnum(String prefVal, String name)
    {
      this.prefVal = prefVal;
      this.name = name;
    }

    static LaunchCommandTypeEnum getByPrefVal(String prefVal)
    {
      return findFirst(EnumSet.allOf(LaunchCommandTypeEnum.class),
                       typeEnum -> typeEnum.prefVal.equals(prefVal));
    }
  }

//---------------------------------------------------------------------------

  private enum OperatingSystemEnum
  {
    windows, mac, linux, other;

    private static OperatingSystemEnum determine()
    {
      if (SystemUtils.IS_OS_WINDOWS) return windows;
      if (SystemUtils.IS_OS_MAC    ) return mac;
      if (SystemUtils.IS_OS_LINUX  ) return linux;

      return other;
    }
  }

//---------------------------------------------------------------------------

  private record Preset(OperatingSystemEnum opSys, String name, LaunchCommandTypeEnum commandType,
                        String commandsPrefKey, String commands)
  {
    private boolean isCompatible() { return opSys == OperatingSystemEnum.determine(); }
  }

//---------------------------------------------------------------------------

  private static final String appPathVar = "::::AppPath", filePathVar = "::::FilePath", pageNumVar = "::::PageNum";

  private final String appPrefKey, commandsPrefKey, commandTypePrefKey;

  private static final StringConverter<LaunchCommandTypeEnum> typeStrConv = new StringConverter<>()
  {
    @Override public String toString(LaunchCommandTypeEnum commandTypeEnum)
    { return commandTypeEnum == null ? "" : commandTypeEnum.name; }

    @Override public LaunchCommandTypeEnum fromString(String string)
    { return findFirst(EnumSet.allOf(LaunchCommandTypeEnum.class), typeEnum -> typeEnum.name.equals(string)); }
  };

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  LaunchCommandsDlgCtrlr(String title, String appPrefKey, String commandsPrefKey, String commandTypePrefKey)
  {
    super("settings/LaunchCommandsDlg", title, true, true);

    this.appPrefKey = appPrefKey;
    this.commandsPrefKey = commandsPrefKey;
    this.commandTypePrefKey = commandTypePrefKey;

    onShown = () -> disableCache(taCommands);

    btnBrowse.setOnAction(event -> SettingsDlgCtrlr.browseClick(tfPath));

    taCommands.setText(app.prefs.get(commandsPrefKey, ""));
    tfPath.setText(app.prefs.get(appPrefKey, ""));

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
        if (confirmDialog("Replace current settings with selected preset?", false) == false)
          return;

      Preset preset = lvPresets.getSelectionModel().getSelectedItem();
      taCommands.setText(preset.commands);
      cbCommandType.getSelectionModel().select(preset.commandType);
    });

    String typePrefVal = app.prefs.get(commandTypePrefKey, opSysCmdAndArgs.prefVal);

    cbCommandType.setItems(FXCollections.observableArrayList(EnumSet.allOf(LaunchCommandTypeEnum.class)));
    cbCommandType.setConverter(typeStrConv);

    cbCommandType.getSelectionModel().select(getByPrefVal(typePrefVal));

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
    app.prefs.put(appPrefKey, tfPath.getText());
    app.prefs.put(commandsPrefKey, taCommands.getText());
    app.prefs.put(commandTypePrefKey, nullSwitch(cbCommandType.getSelectionModel().getSelectedItem(), "", typeEnum -> typeEnum.prefVal));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launch(String appPath, FilePath filePath, String commandsPrefKey, String commandTypePrefKey, int pageNum)
  {
    String commands = app.prefs.get(commandsPrefKey, "");
    LaunchCommandTypeEnum commandType = getByPrefVal(app.prefs.get(commandTypePrefKey, ""));

    if ((commandType != null) && (commands.length() > 0))
    {
      if ((commandType == appleScript) && SystemUtils.IS_OS_MAC)
      {
        String[] argz = { "osascript",
                          "-e",
                          resolve(commands, appPath, filePath, pageNum) };
        try
        {
          Runtime.getRuntime().exec(argz);
        }
        catch (IOException e)
        {
          errorPopup("An error occurred while trying to start application: " + getThrowableMessage(e));
        }

        return;
      }

      if (commandType == opSysCmdAndArgs)
      {
        List<String> list = convertMultiLineStrToStrList(resolve(commands, appPath, filePath, pageNum), false);

        if (list.size() > 1)
        {
          DesktopUtil.exec(true, false, new StringBuilder(), list);
          return;
        }
      }
    }

    DesktopUtil.exec(true, false, new StringBuilder(), appPath, filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String resolve(String commands, String appPath, FilePath filePath, int pageNum)
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

  private static final List<Preset> presets = List.of(

    new Preset(OperatingSystemEnum.windows, "Adobe Acrobat", opSysCmdAndArgs, PrefKey.PDF_READER_COMMANDS,
               appPathVar + '\n' +
               "/A\n" +
               "page=" + pageNumVar + '\n' +
               filePathVar),

    new Preset(OperatingSystemEnum.mac, "Preview (macOS)", appleScript, PrefKey.PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\"\n" +
               "end tell\n" +
               "tell application \"System Events\"\n" +
               "  keystroke \"g\" using {option down, command down}\n" +
               "  keystroke " + pageNumVar + '\n' +
               "  keystroke return\n" +
               "end tell"),

    new Preset(OperatingSystemEnum.mac, "Adobe Acrobat", appleScript, PrefKey.PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\" options \"page=" + pageNumVar + "\"\n" +
               "end tell"));

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
