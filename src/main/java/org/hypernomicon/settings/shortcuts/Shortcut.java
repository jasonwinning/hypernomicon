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

package org.hypernomicon.settings.shortcuts;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.prefs.Preferences;

import static org.hypernomicon.App.app;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.EnumBasedTable;
import org.hypernomicon.view.cellValues.AbstractHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;

import com.google.common.collect.Table;

import javafx.scene.input.*;

//---------------------------------------------------------------------------

public record Shortcut(ShortcutContext context, ShortcutAction action, KeyCombo keyCombo)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum ShortcutContext
  {
    MainWindow             ("mainWindow"      , "Main Window"              , true , true , true ),
    AllWindows             ("allWindows"      , "All Windows"              , true , true , true ),
    FileManager            ("fileManager"     , "File Manager"             , false, true , false),
    BibManager             ("bibManager"      , "Bibliography Manager"     , false, false, false),
    RecordDescription      ("recordDesc"      , "Record Description"       , true , true , true ),
    RecordDescriptionEditor("recordDescEditor", "Record Description Editor", true , false, true ),
    PreviewWindow          ("previewWindow"   , "Preview Window"           , false, false, false),
    ContentsWindow         ("contentsWindow"  , "Contents Window"          , false, false, false),
    QueriesTab             ("queriesTab"      , "Queries Tab"              , true , true , false),
    TreeTab                ("treeTab"         , "Tree Tab"                 , true , true , false),
    PersonsTab             ("personsTab"      , "Persons Tab"              , true , true , true ),
    InstitutionsTab        ("institutionsTab" , "Institutions Tab"         , true , false, false),
    WorksTab               ("worksTab"        , "Works Tab"                , true , true , true ),
    MiscFilesTab           ("miscFilesTab"    , "Misc. Files Tab"          , true , true , true ),
    DebatesTab             ("debatesTab"      , "Problems/Debates Tab"     , true , true , true ),
    PositionsTab           ("positionsTab"    , "Positions Tab"            , true , true , true ),
    ArgumentsTab           ("argumentsTab"    , "Arguments/Stances Tab"    , true , true , true ),
    NotesTab               ("notesTab"        , "Notes Tab"                , true , true , true ),
    TermsTab               ("termsTab"        , "Terms Tab"                , true , true , true );

    private final String prefVal;
    private final boolean isInMainWindow, hasDescView, hasDescEditor;
    final String userReadableName;

    ShortcutContext(String prefVal, String userReadableName, boolean isInMainWindow, boolean hasDescView, boolean hasDescEditor)
    {
      this.prefVal = prefVal;
      this.userReadableName = userReadableName;

      this.isInMainWindow = isInMainWindow;
      this.hasDescView = hasDescView;
      this.hasDescEditor = hasDescEditor;
    }

  //---------------------------------------------------------------------------

    private boolean includes(ShortcutContext other)
    {
      if (other == null) return false;
      if (this == other) return true;

      return switch (this)
      {
        case AllWindows -> true;
        case MainWindow -> other.isInMainWindow;
        case RecordDescription -> other.hasDescView;
        case RecordDescriptionEditor -> other.hasDescEditor;
        default -> false;
      };
    }

  //---------------------------------------------------------------------------

    boolean overlaps(ShortcutContext other)
    {
      return (other != null) && (this.includes(other) || other.includes(this));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum ShortcutAction
  {
    PreviewRecord         ("previewRecord"       , "View selected record/file in Preview Window"),
    ShowMentions          ("showMentions"        , "Show records mentioning current record"),

    CreateNewRecord       ("createNewRecord"     , "Create new record of current type"),
    CreateNewPerson       ("createNewPerson"     , "Create new Person record"),
    CreateNewInstitution  ("createNewInstitution", "Create new Institution record"),
    CreateNewWork         ("createNewWork"       , "Create new Work record"),
    CreateNewMiscFile     ("createNewMiscFile"   , "Create new Misc. File record"),
    CreateNewDebate       ("createNewDebate"     , "Create new Problem/Debate record"),
    CreateNewPosition     ("createNewPosition"   , "Create new Position record"),
    CreateNewArgument     ("createNewArgument"   , "Create new Argument/Stance record"),
    CreateNewNote         ("createNewNote"       , "Create new Note record"),
    CreateNewTerm         ("createNewTerm"       , "Create new Term record"),
    CreateNewInvestigation("createNewInv"        , "Create new Investigation record"),

    GoToMainWindow        ("goToMainWindow"      , "Switch to Main Window"),
    GoToFileManager       ("goToFileManager"     , "Open File Manager"),
    GoToBibManager        ("goToBibManager"      , "Open Bibliography Manager"),
    GoToPreviewWindow     ("goToPreviewWindow"   , "Open Preview Window"),

    GoToPersonsTab        ("goToPersonsTab"      , "Switch to Persons Tab"),
    GoToInstitutionsTab   ("goToInstitutionsTab" , "Switch to Institutions Tab"),
    GoToWorksTab          ("goToWorksTab"        , "Switch to Works Tab"),
    GoToMiscFilesTab      ("goToMiscFilesTab"    , "Switch to Misc. Files Tab"),
    GoToDebatesTab        ("goToDebatesTab"      , "Switch to Problems/Debates Tab"),
    GoToPositionsTab      ("goToPositionsTab"    , "Switch to Positions Tab"),
    GoToArgumentsTab      ("goToArgumentsTab"    , "Switch to Arguments/Stances Tab"),
    GoToNotesTab          ("goToNotesTab"        , "Switch to Notes Tab"),
    GoToTermsTab          ("goToTermsTab"        , "Switch to Terms Tab"),
    GoToQueriesTab        ("goToQueriesTab"      , "Switch to Queries Tab"),
    GoToTreeTab           ("goToTreeTab"         , "Switch to Tree Tab");

    private final String prefVal;
    final String userReadableName;

    ShortcutAction(String prefVal, String userReadableName)
    {
      this.prefVal = prefVal;
      this.userReadableName = userReadableName;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class KeyCombo
  {
    final boolean primary,        // Ctrl (Win/Linux) or Command (macOS)
                  altOrOption,    // Alt or Option
                  shift,          // Shift
                  secondaryCtrl,  // macOS Control (⌃)
                  superKey;       // Linux Super (Windows key)

    public final KeyCode keyCode; // Selected non-modifier key (display label)

  //---------------------------------------------------------------------------

    KeyCombo(KeyCode keyCode, boolean primary, boolean altOrOption, boolean shift, boolean secondaryCtrl, boolean superKey)
    {
      this.keyCode = keyCode;

      this.primary = primary;
      this.altOrOption = altOrOption;
      this.shift = shift;
      this.secondaryCtrl = secondaryCtrl;
      this.superKey = superKey;
    }

  //---------------------------------------------------------------------------

    @Override public String toString()
    {
      if (keyCode == null)
        return "";

      Collection<String> parts = new ArrayList<>();

      switch (CURRENT_OS)
      {
        case MAC:

          if (primary)       parts.add("⌘"); // Command
          if (shift)         parts.add("⇧"); // Shift
          if (altOrOption)   parts.add("⌥"); // Option
          if (secondaryCtrl) parts.add("⌃"); // Control

          // mac rarely uses Super; we hide it anyway.

          parts.add(normalizeKeyLabelForMac(keyCode, false));
          return String.join("", parts);

        case LINUX:

          if (primary    ) parts.add("Ctrl");
          if (shift      ) parts.add("Shift");
          if (altOrOption) parts.add("Alt");
          if (superKey   ) parts.add("Super");

          parts.add(keyCode.getName());
          return String.join("+", parts);

        case WINDOWS:
        default:

          if (primary    ) parts.add("Ctrl");
          if (shift      ) parts.add("Shift");
          if (altOrOption) parts.add("Alt");
          if (superKey   ) parts.add("Win");  // This is not actually selectable in the UI

          parts.add(keyCode.getName());
          return String.join("+", parts);
      }
    }

  //---------------------------------------------------------------------------

    @Override public boolean equals(Object o)
    {
      if (this == o)
        return true;

      if ((o instanceof KeyCombo) == false)
        return false;

      KeyCombo other = (KeyCombo) o;
      return primary == other.primary
          && altOrOption == other.altOrOption
          && shift == other.shift
          && secondaryCtrl == other.secondaryCtrl
          && superKey == other.superKey
          && keyCode == other.keyCode;
    }

  //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      int result = Boolean.hashCode(primary);
      result = 31 * result + Boolean.hashCode(altOrOption);
      result = 31 * result + Boolean.hashCode(shift);
      result = 31 * result + Boolean.hashCode(secondaryCtrl);
      result = 31 * result + Boolean.hashCode(superKey);
      result = 31 * result + (keyCode != null ? keyCode.hashCode() : 0);
      return result;
    }

  //---------------------------------------------------------------------------

    public KeyCombination toJfxKeyCombination()
    {
      if (keyCode == null)
        return null;

      Collection<KeyCombination.Modifier> mods = new ArrayList<>();

      if (primary      ) mods.add(KeyCombination.SHORTCUT_DOWN); // Ctrl or Command
      if (shift        ) mods.add(KeyCombination.SHIFT_DOWN   );
      if (altOrOption  ) mods.add(KeyCombination.ALT_DOWN     );
      if (secondaryCtrl) mods.add(KeyCombination.CONTROL_DOWN ); // macOS Control (⌃)
      if (superKey     ) mods.add(KeyCombination.META_DOWN    ); // Win/Super key

      return new KeyCodeCombination(keyCode, mods.toArray(KeyCombination.Modifier[]::new));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class ShortcutHTC extends AbstractHTC
  {
    final Shortcut shortcut;
    private final int id;

    private ShortcutHTC(Shortcut shortcut)
    {
      this(shortcut, 1);  // Set ID to 1 so this cell isn't treated as equal to GenericNonRecordHTC.blankCell
    }                     // It shouldn't be equal because this cell will have context and action information even if keyCombo is blank

    private ShortcutHTC(Shortcut shortcut, int id)
    {
      super(false);

      this.shortcut = shortcut;
      this.id = id;
    }

    @Override public int getID()                             { return id; }
    @Override public String getText()                        { return shortcut.keyCombo == null ? "" : shortcut.keyCombo.toString(); }
    @Override public RecordType getRecordType()              { return RecordType.hdtNone; }
    @Override public HyperTableCell getCopyWithID(int newID) { throw new UnsupportedOperationException("copy"); }

    @Override public boolean isEmpty()
    {
      return (shortcut == null) || ((shortcut.action == null) && (shortcut.context == null) && strNullOrBlank(getText()));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperTableCell toHTC()                { return new ShortcutHTC(this); }
  boolean conflictsWith(Shortcut other) { return (other != null) && Objects.equals(keyCombo, other.keyCombo) && context.overlaps(other.context); }

  Shortcut copyWithNewKeyCombo(KeyCombo combo) { return new Shortcut(context, action, combo); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String normalizeKeyLabelForMac(KeyCode keyCode, boolean includeWord)
  {
    String symbol = switch (keyCode)
    {
      case ENTER       -> "⏎"; // Return
      case TAB         -> "⇥"; // Tab
      case BACK_SPACE  -> "⌫"; // Delete (backward)
      case DELETE      -> "⌦"; // Forward Delete
      case ESCAPE      -> "⎋"; // Escape
      case SPACE       -> "␣"; // Space (optional, U+2423)
      case PAGE_UP     -> "⇞"; // Page Up
      case PAGE_DOWN   -> "⇟"; // Page Down
      case HOME        -> "↖"; // Home
      case END         -> "↘"; // End
      case CLEAR       -> "⌧"; // Clear (numpad)
      case HELP        -> "⍰"; // Help (rare)
      case LEFT        -> "←"; // Left Arrow
      case RIGHT       -> "→"; // Right Arrow
      case UP          -> "↑"; // Up Arrow
      case DOWN        -> "↓"; // Down Arrow
      default          -> "";
    };

    String word = switch (keyCode)
    {
      case ENTER      -> "Return";
      case BACK_SPACE -> "Delete";
      case DELETE     -> "Forward Delete";
      default         -> keyCode.getName();
    };

    return symbol.isEmpty() ? word : (includeWord ? (symbol + ' ' + word) : symbol);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Table<ShortcutContext, ShortcutAction, Shortcut> loadFromPrefs()
  {
    Table<ShortcutContext, ShortcutAction, Shortcut> shortcuts = new EnumBasedTable<>(ShortcutContext.class, ShortcutAction.class);

    Preferences node = app.prefs.node("shortcuts");

    for (ShortcutContext ctx : ShortcutContext.values())
    {
      for (ShortcutAction act : ShortcutAction.values())
      {
        String stored = node.get(ctx.prefVal + '.' + act.prefVal, null);

        if (strNotNullOrBlank(stored))
        {
          String[] parts = stored.split("\\|");
          if (parts.length == 6)
          {
            try
            {
              KeyCode code = KeyCode.valueOf(parts[0]);
              boolean primary       = Boolean.parseBoolean(parts[1]),
                      altOrOption   = Boolean.parseBoolean(parts[2]),
                      shift         = Boolean.parseBoolean(parts[3]),
                      secondaryCtrl = Boolean.parseBoolean(parts[4]),
                      superKey      = Boolean.parseBoolean(parts[5]);

              KeyCombo combo = new KeyCombo(code, primary, altOrOption, shift, secondaryCtrl, superKey);
              shortcuts.put(ctx, act, new Shortcut(ctx, act, combo));
            }
            catch (IllegalArgumentException ex)
            {
              // Invalid KeyCode name in prefs; skip
            }
          }
        }
      }
    }

    return shortcuts;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void saveToPrefs(Table<ShortcutContext, ShortcutAction, Shortcut> table)
  {
    Preferences node = app.prefs.node("shortcuts");

    for (ShortcutContext ctx : ShortcutContext.values())
    {
      for (ShortcutAction act : ShortcutAction.values())
      {
        Shortcut sc = table.get(ctx, act);

        if ((sc != null) && (sc.keyCombo != null) && (sc.keyCombo.keyCode != null))
        {
          // Serialize as: keyCodeName|primary|altOrOption|shift|secondaryCtrl|superKey
          KeyCombo kc = sc.keyCombo;
          String value = kc.keyCode.name() + '|'
                       + kc.primary + '|'
                       + kc.altOrOption + '|'
                       + kc.shift + '|'
                       + kc.secondaryCtrl + '|'
                       + kc.superKey;

          node.put(ctx.prefVal + '.' + act.prefVal, value);
        }
        else
        {
          // Remove if no shortcut assigned
          node.remove(ctx.prefVal + '.' + act.prefVal);
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
