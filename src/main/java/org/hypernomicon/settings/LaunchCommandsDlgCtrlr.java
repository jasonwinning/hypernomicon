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

package org.hypernomicon.settings;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.view.wrappers.SimpleSelector;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.settings.LaunchCommandsDlgCtrlr.LaunchCommandTypeEnum.*;

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.DesktopUtil.OperatingSystem.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;

import javafx.fxml.FXML;
import javafx.scene.control.*;

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

  enum LaunchCommandTypeEnum
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

  private record Preset(OperatingSystem opSys, String name, LaunchCommandTypeEnum commandType,
                        String commandsPrefKey, String commands)
  {
    private boolean isCompatible() { return opSys == CURRENT_OS; }
  }

//---------------------------------------------------------------------------

  private static final String appPathVar = "::::AppPath", filePathVar = "::::FilePath", pageNumVar = "::::PageNum";

  private final String appPrefKey, commandsPrefKey, commandTypePrefKey;

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

    SequencedMap<LaunchCommandTypeEnum, String> strMap = new LinkedHashMap<>();

    for (var commandTypeEnum : LaunchCommandTypeEnum.values())
      strMap.put(commandTypeEnum, commandTypeEnum.name);

    SimpleSelector.init(cbCommandType, strMap);

    cbCommandType.getSelectionModel().select(getByPrefVal(typePrefVal));

    presets.stream().filter(preset -> preset.isCompatible() && preset.commandsPrefKey.equals(commandsPrefKey))
                    .forEach(preset -> lvPresets.getItems().add(preset));
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
      if ((commandType == appleScript) && IS_OS_MAC)
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
          exec(true, false, new StringBuilder(), list);
          return;
        }
      }
    }

    exec(true, false, new StringBuilder(), appPath, filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String resolve(String commands, CharSequence appPath, FilePath filePath, int pageNum)
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

  private static final List<Preset> presets = List.of
  (
    new Preset(WINDOWS, "Adobe Acrobat", opSysCmdAndArgs, PrefKey.PDF_READER_COMMANDS,
               appPathVar + '\n' +
               "/A\n" +
               "page=" + pageNumVar + '\n' +
               filePathVar),

    new Preset(WINDOWS, "Sumatra", opSysCmdAndArgs, PrefKey.PDF_READER_COMMANDS,
               appPathVar + '\n' +
               "-page\n" +
               pageNumVar + '\n' +
               filePathVar),

    new Preset(MAC, "Preview (macOS)", appleScript, PrefKey.PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\"\n" +
               "end tell\n" +
               "tell application \"System Events\"\n" +
               "  keystroke \"g\" using {option down, command down}\n" +
               "  keystroke " + pageNumVar + '\n' +
               "  keystroke return\n" +
               "end tell"),

    new Preset(MAC, "Adobe Acrobat", appleScript, PrefKey.PDF_READER_COMMANDS,
               "tell app \"" + appPathVar + "\"\n" +
               "  activate\n" +
               "  open \"" + filePathVar + "\" options \"page=" + pageNumVar + "\"\n" +
               "end tell")
  );

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
