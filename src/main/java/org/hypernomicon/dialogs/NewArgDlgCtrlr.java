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

package org.hypernomicon.dialogs;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.*;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Verdict.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.HDT_Verdict.HDT_PositionVerdict;
import org.hypernomicon.model.searchKeys.KeywordLinkScanner;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.settings.ArgumentNamingSettings;
import org.hypernomicon.testTools.FXTestSequencer;
import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.SimpleSelector;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.web.WebView;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

public class NewArgDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkIncludeAuth, chkLowerCaseTargetName;
  @FXML private ComboBox<HyperTableCell> cbPerson, cbVerdict, cbWork;
  @FXML private ComboBox<Ternary> cbArgOrStance;
  @FXML private Label lblTargetName, lblTargetDesc, lblDoesWhat;
  @FXML private RadioButton rbArgName1, rbArgName2, rbArgName3, rbArgName4, rbArgName5, rbArgName6, rbArgName7, rbArgName8, rbExisting, rbNew, rbNone;
  @FXML private TextField tfArgName1, tfArgName2, tfArgName3, tfArgName4, tfArgName5, tfArgName6, tfArgName7, tfArgName8, tfTargetName, tfTitle, tfPages;
  @FXML private WebView webView;

  private final HDT_RecordWithMainText target;
  private final HyperCB hcbPerson, hcbVerdict, hcbWork;
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);

  private HDT_Argument argument;
  private boolean revising = false, programmaticWorkChange = false, programmaticVerdictChange = false;

  private final Map<TextField, String> textFieldToLastGen = new HashMap<>();

  public HDT_Argument getArgument() { return argument; }

//---------------------------------------------------------------------------

  public NewArgDlgCtrlr(HDT_Position position)
  {
    this("New Argument/Stance", position);
  }

  public NewArgDlgCtrlr(HDT_Argument targetArg)
  {
    this("New Response Argument", targetArg);
  }

  private NewArgDlgCtrlr(String title, HDT_RecordWithMainText target)
  {
    super("NewArgDlg", title, true);

    this.target = target;
    RecordType verdictType = target.getType() == hdtPosition ? hdtPositionVerdict : hdtArgumentVerdict;

    hcbPerson  = new HyperCB(cbPerson , ctEditableLimitedDropDown, new StandardPopulator     (hdtPerson));
    hcbVerdict = new HyperCB(cbVerdict, ctEditableLimitedDropDown, new StandardPopulator     (verdictType));
    hcbWork    = new HyperCB(cbWork   , ctEditableLimitedDropDown, new HybridSubjectPopulator(rtAuthorOfWork));

    rbArgName1.setSelected(true);

    hcbWork.addListener((oldCell, newCell) ->
    {
      if (programmaticWorkChange) return;

      rbExisting.setSelected(true);

      HDT_Work work = HyperTableCell.getRecord(newCell);

      if ((work != null) && (hcbPerson.selectedID() == -1))
      {
        HDT_Person person = findFirst(work.getAuthors(), author -> (author.getPerson() != null) && author.getInFileName().isTrue(), Author::getPerson);

        if (person == null)
        {
          person = findFirst(work.getAuthors(), author -> (author.getPerson() != null) && (author.getInFileName() != Ternary.False), Author::getPerson);

          if (person == null)
            person = findFirst(work.getAuthors(), author -> author.getPerson() != null, Author::getPerson);
        }

        if (person != null)
        {
          programmaticWorkChange = true;
          hcbPerson.selectIDofRecord(person);

          Platform.runLater(() ->
          {
            hcbWork.selectIDofRecord(work);
            programmaticWorkChange = false;
            reviseSuggestions(cbArgOrStance.getValue());
          });

          return;
        }
      }

      reviseSuggestions(cbArgOrStance.getValue());
    });

    String noun = target.getType() == hdtPosition ? "Position" : "Target argument";
    lblTargetName.setText(noun + ':');
    lblTargetDesc.setText(noun + " description:");

    tfTitle.setTextFormatter(WorkDlgCtrlr.titleFormatter(alreadyChangingTitle));

    tfTitle.textProperty().addListener((ob, ov, nv) -> rbNew.setSelected(true));

    chkIncludeAuth.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions(cbArgOrStance.getValue()));

    chkLowerCaseTargetName.setSelected(db.prefs.getBoolean(PrefKey.LOWER_CASE_TARGET_NAMES, false));

    chkLowerCaseTargetName.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions(cbArgOrStance.getValue()));

    hcbVerdict.selectID(getVerdictID(verdictType == hdtPositionVerdict, verdictType == hdtPositionVerdict));

    if (verdictType == hdtPositionVerdict) hcbVerdict.addListener((ov, nv) ->
    {
      if (programmaticVerdictChange) return;

      HDT_PositionVerdict verdict = HyperTableCell.getRecord(nv);
      if (verdict == null) return;

      if (verdict.isInFavor().isFalse())
      {
        if      (rbArgName1.isSelected() && tfArgName1.getText().equals(textFieldToLastGen.get(tfArgName1))) rbArgName5.setSelected(true);
        else if (rbArgName2.isSelected() && tfArgName2.getText().equals(textFieldToLastGen.get(tfArgName2))) rbArgName6.setSelected(true);
        else if (rbArgName3.isSelected() && tfArgName3.getText().equals(textFieldToLastGen.get(tfArgName3))) rbArgName7.setSelected(true);
        else if (rbArgName4.isSelected() && tfArgName4.getText().equals(textFieldToLastGen.get(tfArgName4))) rbArgName8.setSelected(true);
      }
      else
      {
        if      (rbArgName5.isSelected() && tfArgName5.getText().equals(textFieldToLastGen.get(tfArgName5))) rbArgName1.setSelected(true);
        else if (rbArgName6.isSelected() && tfArgName6.getText().equals(textFieldToLastGen.get(tfArgName6))) rbArgName2.setSelected(true);
        else if (rbArgName7.isSelected() && tfArgName7.getText().equals(textFieldToLastGen.get(tfArgName7))) rbArgName3.setSelected(true);
        else if (rbArgName8.isSelected() && tfArgName8.getText().equals(textFieldToLastGen.get(tfArgName8))) rbArgName4.setSelected(true);
      }
    });

    tfTargetName.setText(target.name());

    webView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));

    MainTextWrapper.setReadOnlyHTML(target.getMainText().getHtml(), webView.getEngine());

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    rbNew.setSelected(true);

    rbExisting.getToggleGroup().selectedToggleProperty().addListener((ob, ov, nv) ->
    {
      // The Work selection only affects the name suggestions when "Existing" is chosen, because that
      // is the only case where author names are pulled from the work instead of the Person field.
      // Switching between "New" and "None" leaves the suggestions identical, so skip regenerating them

      if ((ov == rbExisting) || (nv == rbExisting))
        reviseSuggestions(cbArgOrStance.getValue());
    });

    alreadyChangingTitle.setTrue();
    tfTitle.setText(target.name() + (target.getType() == hdtPosition ? " Argument/Stance Stem" : " Counterargument Stem"));
    alreadyChangingTitle.setFalse();

    addListeners(tfArgName1, rbArgName1, true ); addListeners(tfArgName2, rbArgName2, true );
    addListeners(tfArgName3, rbArgName3, true ); addListeners(tfArgName4, rbArgName4, true );
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    hcbPerson.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      // When the person was filled in automatically because the user chose an existing work, only
      // refilter the work choices; clearing the work and reverting to "New" should happen only when
      // the user changed the person directly.

      boolean autoFilledFromWork = programmaticWorkChange;

      Platform.runLater(() ->
      {
        ((HybridSubjectPopulator) hcbWork.getPopulator()).setObj(HyperTableCell.getRecord(newCell));
        hcbWork.populate(true);

        if (autoFilledFromWork == false)
        {
          hcbWork.selectID(-1);
          rbNew.setSelected(true);
        }

        reviseSuggestions(cbArgOrStance.getValue());
      });
    });

    chkIncludeAuth.setSelected(true);

    SequencedMap<Ternary, String> strMap = new LinkedHashMap<>();

    strMap.put(Ternary.False, "Stance");
    strMap.put(Ternary.True , "Argument");

    SimpleSelector.init(cbArgOrStance, strMap);

    cbArgOrStance.setValue(Ternary.True);

    if (target.getType() != hdtPosition)
      cbArgOrStance.setDisable(true);

    cbArgOrStance.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) ->
    {
      if (Ternary.isNullOrUnset(nv) == false)
        reviseSuggestions(nv);
    });

    reviseSuggestions(Ternary.True);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addListeners(TextField tf, RadioButton rb, boolean proArg)
  {
    rb.selectedProperty().addListener((ob, oldSelected, newSelected) -> argNameSelect(newSelected, proArg));

    tf.textProperty().addListener((ob, oldText, newText) ->
    {
      if (revising == false)
        rb.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void argNameSelect(Boolean newSelected, boolean proArg)
  {
    if (target.getType() != hdtPosition)
      return;

    if (Boolean.TRUE.equals(newSelected))
    {
      programmaticVerdictChange = true;

      hcbVerdict.selectID(getVerdictID(proArg, true));

      programmaticVerdictChange = false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int getVerdictID(boolean isInFavor, boolean isPosVerdict)
  {
    RecordType type = isPosVerdict ? hdtPositionVerdict : hdtArgumentVerdict;

    for (HDT_Record record : db.records(type))
    {
      HDT_Verdict verdict = (HDT_Verdict) record;

      if (isInFavor ? verdict.isInFavor().isTrue() : verdict.isInFavor().isFalse())
        return verdict.getID();
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reviseSuggestions(Ternary isArgument)
  {
    String peoplePart = "";

    revising = true;

    if      (isArgument == Ternary.True ) lblDoesWhat.setText("Argues that:");
    else if (isArgument == Ternary.False) lblDoesWhat.setText("Holds that:" );

    if (chkIncludeAuth.isSelected())
      peoplePart = getAuthorNamesForSuggestions();

    String targetName = target.name();
    if (targetName.startsWith("The "))
      targetName = "the " + targetName.substring(4);

    if (chkLowerCaseTargetName.isSelected())
    {
      List<int[]> personSpans = KeywordLinkScanner.scan(targetName).stream()
        .filter(link -> link.recordStream().anyMatch(r -> r.getType() == hdtPerson))
        .map(link -> new int[] { link.getOffset(), link.getLength() })
        .toList();

      targetName = lowerCasePreservingSpans(targetName, personSpans);
    }

    if (target.getType() == hdtPosition)
    {
      chkLowerCaseTargetName.setText("Lower case position name");

      if (isArgument == Ternary.False)
      {
        setArgTextField(tfArgName1, peoplePart, "Endorsement of "              , targetName);
        setArgTextField(tfArgName2, peoplePart, "Endorsement of the "          , targetName);
        setArgTextField(tfArgName3, peoplePart, "Stance that "                 , targetName);
        setArgTextField(tfArgName4, peoplePart, "Stance affirming that "       , targetName);
        setArgTextField(tfArgName5, peoplePart, "Rejection of "                , targetName);
        setArgTextField(tfArgName6, peoplePart, "Rejection of the "            , targetName);
        setArgTextField(tfArgName7, peoplePart, "Rejection of the claim that " , targetName);
        setArgTextField(tfArgName8, peoplePart, "Stance against the view that ", targetName);
      }
      else
      {
        setArgTextField(tfArgName1, peoplePart, "Argument for "                  , targetName);
        setArgTextField(tfArgName2, peoplePart, "Argument for the "              , targetName);
        setArgTextField(tfArgName3, peoplePart, "Argument that "                 , targetName);
        setArgTextField(tfArgName4, peoplePart, "Argument for the view that "    , targetName);
        setArgTextField(tfArgName5, peoplePart, "Argument against "              , targetName);
        setArgTextField(tfArgName6, peoplePart, "Argument against the "          , targetName);
        setArgTextField(tfArgName7, peoplePart, "Argument that it is false that ", targetName);
        setArgTextField(tfArgName8, peoplePart, "Argument against the view that ", targetName);
      }
    }
    else
    {
      chkLowerCaseTargetName.setText("Lower case target name");

      setArgTextField(tfArgName1, peoplePart, "Counterargument against "               , targetName);
      setArgTextField(tfArgName2, peoplePart, "Counterargument against the "           , targetName);
      setArgTextField(tfArgName3, peoplePart, "Counterargument against the claim that ", targetName);

      setArgTextField(tfArgName4, peoplePart, "Response to "    , targetName);
      setArgTextField(tfArgName5, peoplePart, "Response to the ", targetName);

      setArgTextField(tfArgName6, peoplePart, "Objection to "               , targetName);
      setArgTextField(tfArgName7, peoplePart, "Objection to the "           , targetName);
      setArgTextField(tfArgName8, peoplePart, "Objection to the claim that ", targetName);
    }

    revising = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setArgTextField(TextField tf, String peoplePart, String desc, String targetName)
  {
    String str = peoplePart + (strNullOrBlank(peoplePart) ? desc : desc.toLowerCase()) + targetName;

    // Don't clobber a name the user has typed: only push the recommendation into the field when the field
    // still holds the previously generated suggestion, or when the user has cleared it.

    if (tf.getText().isBlank() || tf.getText().equals(textFieldToLastGen.get(tf)))
      tf.setText(str);

    textFieldToLastGen.put(tf, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return {@code text} with every character lowercased, except for the
   * character ranges given by {@code spansToPreserve}, which retain their
   * original case.
   *
   * <p>Each element of {@code spansToPreserve} is an {@code int[]} of
   * {@code [offset, length]}. Spans must be:
   * <ul>
   *   <li>in ascending order by offset,</li>
   *   <li>non-overlapping,</li>
   *   <li>fully contained within {@code text} (offset &gt;= 0, offset + length &lt;= text.length()).</li>
   * </ul>
   *
   * <p>A zero-length span is allowed and preserves nothing (a no-op at that offset).
   * An empty {@code spansToPreserve} list simply returns {@code text.toLowerCase()}.
   *
   * <p>Package-private and static for unit testing. Kept here rather than in a
   * generic utility class because the span-preservation semantics are tied to
   * the argument-naming use case (proper-noun preservation via the KeywordLinkScanner).
   */
  static String lowerCasePreservingSpans(String text, List<int[]> spansToPreserve)
  {
    if (spansToPreserve.isEmpty())
      return text.toLowerCase();

    StringBuilder sb = new StringBuilder(text.length());
    int ndx = 0;

    for (int[] span : spansToPreserve)
    {
      int offset = span[0],
          length = span[1];

      sb.append(text.substring(ndx, offset).toLowerCase());
      sb.append(text, offset, offset + length);

      ndx = offset + length;
    }

    sb.append(text.substring(ndx).toLowerCase());
    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getAuthorNamesForSuggestions()
  {
    HDT_Person person = hcbPerson.selectedRecord();
    HDT_Work work = hcbWork.selectedRecord();

    ArgumentNamingSettings settings = new ArgumentNamingSettings();

    if ((settings.multipleAuthors == false) || (rbExisting.isSelected() == false) || (work == null))
      return person == null ? "" : person.getLastName() + "'s ";

    if (work.getAuthors().isEmpty()) return "";

    String authorNames = settings.format(work.getAuthors().stream().filter(Author::getIsAuthor).map(Author::singleName).toList());

    if (strNullOrBlank(authorNames))
    {
      authorNames = settings.format(work.getAuthors().stream().filter(Author::getIsEditor).map(Author::singleName).toList());

      if (strNullOrBlank(authorNames))
        authorNames = settings.format(work.getAuthors().stream().map(Author::singleName).toList());
    }

    return strNullOrBlank(authorNames) ? "" : (authorNames + "'s ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    HDT_Record verdict = hcbVerdict.selectedRecord();

    if (verdict == null)
      return falseWithErrorPopup("You must select a verdict.", cbVerdict);

    Ternary isArgument = cbArgOrStance.getValue();

    if (Ternary.isNullOrUnset(isArgument))
      return falseWithErrorPopup("You must select either Argument or Stance.", cbArgOrStance);

    // Identify which argument-name text field is selected. The radio buttons
    // share a ToggleGroup so normally exactly one is selected, but guard
    // against "none selected" too.

    TextField selectedNameTf = rbArgName1.isSelected() ? tfArgName1
                             : rbArgName2.isSelected() ? tfArgName2
                             : rbArgName3.isSelected() ? tfArgName3
                             : rbArgName4.isSelected() ? tfArgName4
                             : rbArgName5.isSelected() ? tfArgName5
                             : rbArgName6.isSelected() ? tfArgName6
                             : rbArgName7.isSelected() ? tfArgName7
                             : rbArgName8.isSelected() ? tfArgName8
                                                       : null;

    if (selectedNameTf == null)
      return falseWithErrorPopup("You must select one of the suggested argument names.", tfArgName1);

    if (selectedNameTf.getText().isBlank())
      return falseWithErrorPopup("The selected argument name cannot be empty.", selectedNameTf);

    if (rbNew.isSelected() && tfTitle.getText().isBlank())
      return falseWithErrorPopup("Enter a title for the new work.", tfTitle);

    // All validation passed; create records and populate them.

    argument = db.createNewBlankRecord(hdtArgument);

    argument.setIsArgument(isArgument);

    if (verdict.getType() == hdtPositionVerdict)
      argument.addPosition((HDT_Position)target, (HDT_PositionVerdict)verdict);
    else
    {
      HDT_Argument targetArg = (HDT_Argument)target;

      try { argument.addTargetArg(targetArg, (HDT_ArgumentVerdict)verdict); } catch (RelationCycleException e) { throw newAssertionError(e); }
      targetArg.positions.forEach(position -> argument.addPosition(position, null));
    }

    argument.setName(selectedNameTf.getText());

    HDT_Work work;

    if (rbNew.isSelected())
    {
      work = db.createNewBlankRecord(hdtWork);

      work.setName(tfTitle.getText());
      nullSwitch(hcbPerson.selectedRecord(), (HDT_Person person) -> work.getAuthors().add(person));
    }
    else if (rbExisting.isSelected())
      work = hcbWork.selectedRecord();
    else
      work = null;

    if (work != null)
    {
      argument.works.add(work);
      argument.setPagesInWork(work, tfPages.getText());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run an automated test sequence covering the interaction between the work
   * dropdown, the person dropdown, the New/Existing/None radio buttons, and
   * the name suggestions. Invoked from the Test Console.
   */
  public static void runTests()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    PopupRobot.setActive(true);
    PopupRobot.clear();

    boolean oldMultAuthPref = db.prefs.getBoolean(PrefKey.ARG_NAMING_MULTIPLE_AUTHORS, false);

    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    position.setName("Author Was Right");

    HDT_Person personA = db.createNewBlankRecord(hdtPerson),
               personB = db.createNewBlankRecord(hdtPerson);

    personA.setName(new PersonName("Alice", "Author"));
    personB.setName(new PersonName("Bob", "Bystander"));

    try
    {
      personA.setSearchKey("Author", false);
    }
    catch (SearchKeyException e)
    {
      PopupRobot.setActive(false);

      db.deleteRecord(personA);
      db.deleteRecord(personB);
      db.deleteRecord(position);

      errorPopup("Unable to set search key: " + getThrowableMessage(e));
      return;
    }

    HDT_Work authoredWork = db.createNewBlankRecord(hdtWork),
             editedWork   = db.createNewBlankRecord(hdtWork);

    authoredWork.setName("Authored Work");
    authoredWork.getAuthors().add(personA);
    authoredWork.getAuthors().add(personB);
    authoredWork.setPersonIsEditor(personB, true);

    editedWork.setName("Edited Work");
    editedWork.getAuthors().add(personB);
    editedWork.setPersonIsEditor(personB, true);

    NewArgDlgCtrlr dlg = new NewArgDlgCtrlr(position);

    FXTestSequencer seq = new FXTestSequencer();

    seq.setFinalizer(() -> runDelayedInFXThread(1, 200, () ->
    {
      if (dlg.stage.isShowing())
        dlg.btnCancelClick();

      PopupRobot.setActive(false);
      PopupRobot.clear();

      db.prefs.putBoolean(PrefKey.ARG_NAMING_MULTIPLE_AUTHORS, oldMultAuthPref);

      db.deleteRecord(authoredWork);
      db.deleteRecord(editedWork);
      db.deleteRecord(personA);
      db.deleteRecord(personB);
      db.deleteRecord(position);
    }));

    seq.setDelayMS(400)

      .thenRun(() -> Platform.runLater(dlg::showModal))

    //---------------------------------------------------------------------------

      // Ticket #92: with "Lower case position name" checked, words matching person
      // record search keys keep their capitalization.

      .thenRunAfterDelay(() ->
      {
        assertTrue(dlg.stage.isShowing(), "Dialog should be showing.");
        assertTrue(dlg.rbNew.isSelected(), "\"New\" should be selected initially.");

        dlg.chkLowerCaseTargetName.setSelected(true);
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals("Argument for Author was right", dlg.tfArgName1.getText(), "Lower-casing should preserve the person search key match.");

        dlg.chkLowerCaseTargetName.setSelected(false);
      })

    //---------------------------------------------------------------------------

      // Ticket #104: choose the work while "New" is selected and the Person field
      // is blank. The person should be auto-filled from the work's author, and the
      // radio selection should end up on "Existing".

      .thenRunAfterDelay(() ->
      {
        assertEquals("Argument for Author Was Right", dlg.tfArgName1.getText(), "Suggestion should be restored after unchecking lower-casing.");

        dlg.hcbWork.selectIDofRecord(authoredWork);
      })

      .thenRunAfterDelay(() ->
      {
        assertTrue(dlg.rbExisting.isSelected(), "\"Existing\" should be selected after choosing a work.");
        assertEquals(personA.getID(), dlg.hcbPerson.selectedID(), "Person should be auto-filled from the work's author.");
        assertEquals(authoredWork.getID(), dlg.hcbWork.selectedID(), "Work should stay selected after the person is auto-filled.");
      })

    //---------------------------------------------------------------------------

      // Change the person directly. That should clear the work and revert to "New".

      .thenRun(() -> dlg.hcbPerson.selectIDofRecord(personB))

      .thenRunAfterDelay(() ->
      {
        assertTrue(dlg.rbNew.isSelected(), "\"New\" should be selected after changing the person directly.");
        assertEquals(-1, dlg.hcbWork.selectedID(), "Work should be cleared after changing the person directly.");
      })

    //---------------------------------------------------------------------------

      // Ticket #104: blank the person, select "Existing" explicitly, then choose
      // the work. The radio selection must survive the auto-fill cascade.

      .thenRun(() -> dlg.hcbPerson.selectID(-1))

      .thenRunAfterDelay(() ->
      {
        dlg.rbExisting.setSelected(true);
        dlg.hcbWork.selectIDofRecord(authoredWork);
      })

      .thenRunAfterDelay(() ->
      {
        assertTrue(dlg.rbExisting.isSelected(), "\"Existing\" should stay selected after choosing a work.");
        assertEquals(personA.getID(), dlg.hcbPerson.selectedID(), "Person should be auto-filled from the work's author.");
        assertEquals(authoredWork.getID(), dlg.hcbWork.selectedID(), "Work should stay selected after the person is auto-filled.");
      })

    //---------------------------------------------------------------------------

      // Ticket #98: a manually typed argument name survives switching the Work
      // setting; blanking the field brings the automatic suggestion back.

      .thenRun(() ->
      {
        dlg.tfArgName3.setText("My custom name");
        dlg.rbNone.setSelected(true);
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals("My custom name", dlg.tfArgName3.getText(), "Typed name should survive switching the work setting to \"None\".");

        dlg.tfArgName3.setText("");
        dlg.rbExisting.setSelected(true);
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals(dlg.textFieldToLastGen.get(dlg.tfArgName3), dlg.tfArgName3.getText(), "Automatic suggestion should be restored after blanking the field.");
        assertFalse(dlg.tfArgName3.getText().isBlank(), "Restored suggestion should not be blank.");
      })

    //---------------------------------------------------------------------------

      // Tickets #78 and #98: with multiple-author naming enabled, suggestions use
      // author names only; when the work's people are all editors or translators,
      // editor names are the fallback.

      .thenRun(() ->
      {
        db.prefs.putBoolean(PrefKey.ARG_NAMING_MULTIPLE_AUTHORS, true);

        dlg.hcbPerson.selectID(-1);
      })

      .thenRunAfterDelay(() -> dlg.hcbWork.selectIDofRecord(authoredWork))

      .thenRunAfterDelay(() ->
      {
        assertEquals("Author's argument for Author Was Right", dlg.tfArgName1.getText(), "Suggestion should include the author's name but not the editor's.");

        dlg.hcbPerson.selectID(-1);
      })

      .thenRunAfterDelay(() -> dlg.hcbWork.selectIDofRecord(editedWork))

      .thenRunAfterDelay(() ->
      {
        assertEquals(personB.getID(), dlg.hcbPerson.selectedID(), "Person should be auto-filled from the editor when the work has no authors.");
        assertEquals("Bystander's argument for Author Was Right", dlg.tfArgName1.getText(), "Editor's name should be the fallback when the work's people are all editors.");
      })

    //---------------------------------------------------------------------------

      .thenRun(dlg::btnCancelClick)

      .start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
