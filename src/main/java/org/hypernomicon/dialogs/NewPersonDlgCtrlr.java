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

package org.hypernomicon.dialogs;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.authors.*;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.query.personMatch.PersonForDupCheck;
import org.hypernomicon.query.personMatch.PersonMatcher;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;
import java.util.Objects;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

import org.hypernomicon.view.MainCtrlr;

//---------------------------------------------------------------------------

/**
 * This popup can be used to find duplicates of a given author, present the duplicates to the user,
 * and allow the user to update the author's name and search key, create a person record for the
 * author if one is not yet associated, and merge the author with one of the duplicates.
 * <p>
 * The popup will make whatever update the user has chosen upon closing, except when an author
 * with no associated person record is being updated without being merged. In that case, the
 * caller has to update the author's name.
 * <p>
 * If a set up potential duplicates is passed into origMatches parameter, it will present those
 * to the user. If origMatches is empty, it will search for duplicates immediately upon opening.
 */
public class NewPersonDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apDup;
  @FXML private Label lblDupSearchKey, lblDupTitle, lblDupType, lblDupYear, lblSearchKey, lblStatus;
  @FXML private ProgressIndicator progressIndicator;
  @FXML private RadioButton rbAddNoCreate, rbCreateNoMerge, rbMerge, rbUseDupName, rbUseName;
  @FXML private TabPane tabPane;
  @FXML private TextField tfDupFirstName, tfDupLastName, tfDupSearchKey, tfFirstName, tfLastName, tfSearchKey;
  @FXML private ToggleGroup grpAction, grpName;

  private final RecordAuthor origAuthor;
  private final PersonMatcher matcher;

  private HDT_Person person;
  private boolean alreadyChangingName = false, noTabUpdate = false;
  private HyperTask task;

  public HDT_Person getPerson() { return person; }
  public PersonName getName()   { return new PersonName(tfFirstName.getText(), tfLastName.getText()); }

  public boolean updateWithoutCreateWasSelected() { return rbAddNoCreate.isSelected(); }

  private RecordAuthor curDupAuthor() { return matcher.isEmpty() ? null : matcher.getMatchedAuthor(tabPane.getSelectionModel().getSelectedIndex()); }
  private HDT_Person curDupPerson()   { return nullSwitch(curDupAuthor(), null, Author::getPerson); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create popup window to select how to update or merge authors
   * @param mustCreate If true, the option to update without creating a person record will not be shown, even if
   *                   the author does not have an associated person record.
   * @param name New name to default into the name fields; cannot be null. This name may be different from
   *             the one in origAuthor, if origAuthor was previously saved to its associated work record.
   * @param origAuthor Author object containing original name of author, associated work, and associated HDT_Person record.
   *                   If the work is non-null, this may or may not indicate that the author was previously saved to the work.
   */
  public NewPersonDlgCtrlr(boolean mustCreate, String name, RecordAuthor origAuthor)
  {
    this("Add a New Person to the Database", mustCreate, new PersonName(Objects.requireNonNull(name)), null, origAuthor, List.of());
  }

  /**
   * Create popup window to select how to update or merge authors
   * @param personName New name to default into the name fields; cannot be null. This name may be different from
   *                   the one in origAuthor, if origAuthor was previously saved to its associated work record.
   * @param searchKey New searchKey to default into the name fields; will be generated from personName if null
   * @param origAuthor Author object containing original name of author, associated work, and associated HDT_Person record.
   *                   If the work is non-null, this may or may not indicate that the author was previously saved to the work.
   * @param origMatches Duplicate matches to default in. May not be null but may be empty. If empty, the popup will
   *                    immediately find matches.
   */
  public NewPersonDlgCtrlr(PersonName personName, String searchKey, RecordAuthor origAuthor, List<RecordAuthor> origMatches)
  {
    this("Potential Duplicate(s)", false, personName, searchKey, origAuthor, origMatches);
  }

  /**
   * Create popup window to select how to update or merge authors
   * @param title Title of the popup window
   * @param mustCreate If true, the option to update without creating a person record will not be shown, even if
   *                   the author does not have an associated person record.
   * @param personName New name to default into the name fields; cannot be null. This name may be different from
   *                   the one in origAuthor, if origAuthor was previously saved to its associated work record.
   * @param searchKey New searchKey to default into the name fields; will be generated from personName if null
   * @param origAuthor Author object containing original name of author, associated work, and associated HDT_Person record.
   *                   If the work is non-null, this may or may not indicate that the author was previously saved to the work.
   * @param origMatches Duplicate matches to default in. May not be null but may be empty. If empty, the popup will
   *                    immediately find matches.
   */
  private NewPersonDlgCtrlr(String title, boolean mustCreate, PersonName personName, String searchKey, RecordAuthor origAuthor, List<RecordAuthor> origMatches)
  {
    super("NewPersonDlg", title, true);

    Objects.requireNonNull(personName);
    Objects.requireNonNull(origAuthor);
    Objects.requireNonNull(origMatches);

    this.origAuthor = origAuthor;
    this.person     = origAuthor.getPerson();

    matcher = new PersonMatcher(new PersonForDupCheck(origAuthor, personName), origMatches);

    rbCreateNoMerge.setText(person == null ? "Create Person Record" : "Don't Merge");

    setAllVisible(false, lblStatus, progressIndicator, tabPane);

    tabPane.getSelectionModel().selectedIndexProperty().addListener((ob, oldValue, newValue) ->
    {
      if (noTabUpdate) return;

      tabPane.getTabs().get(oldValue.intValue()).setContent(null);
      tabPane.getTabs().get(newValue.intValue()).setContent(apDup);

      updateCurrentTab();
    });

    setToolTip(lblSearchKey   , "Regenerate search key");
    setToolTip(lblDupSearchKey, "Regenerate search key");

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    lblSearchKey.setOnMouseClicked(event -> setSearchKey(new PersonName(tfFirstName.getText(), tfLastName.getText())));

    lblDupSearchKey.setOnMouseClicked(event -> regenerateDupSearchKey());

    tfFirstName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (alreadyChangingName) return;
      setSearchKey(new PersonName(newValue, tfLastName.getText()));
      startDupThread(new PersonName(newValue, tfLastName.getText()));
    });

    if (mustCreate || (person != null) || (origAuthor.getWork() == null))
    {
      rbAddNoCreate.setDisable(true);
      rbCreateNoMerge.setSelected(true);
    }

    grpName.selectedToggleProperty().addListener((ob, ov, nv) ->
    {
      if ((nv != null) && (grpAction.getSelectedToggle() == null))
        rbMerge.setSelected(true);
    });

    grpAction.selectedToggleProperty().addListener((ob, ov, nv) -> updateRadioButtons());

    tfSearchKey.disableProperty().bind(rbAddNoCreate.selectedProperty());

    tfLastName.setTextFormatter(new TextFormatter<>(change ->
    {
      if (alreadyChangingName) return change;

      if (change.getText().length() > 1)
      {
        if (tfFirstName.getText().isEmpty() && change.getControlText().isEmpty())
        {
          alreadyChangingName = true;
          String newText = change.getControlNewText();

          PersonName pName = new PersonName(newText);
          tfFirstName.setText(pName.getFirst());

          change.setRange(0, change.getControlText().length());
          change.setText(pName.getLast());

          alreadyChangingName = false;
          setSearchKey(new PersonName(tfFirstName.getText(), change.getControlNewText()));
          startDupThread(new PersonName(tfFirstName.getText(), change.getControlNewText()));
          return change;
        }
      }

      if (change.isContentChange())
      {
        setSearchKey(new PersonName(tfFirstName.getText(), change.getControlNewText()));
        startDupThread(new PersonName(tfFirstName.getText(), change.getControlNewText()));
      }

      return change;
    }));

    stage.setOnHidden(event -> stopDupThread());

    alreadyChangingName = true;

    tfFirstName.setText(personName.getFirst());
    tfLastName .setText(personName.getLast ());

    if (searchKey == null)
      setSearchKey(personName);
    else
      tfSearchKey.setText(searchKey);

    alreadyChangingName = false;

    if (matcher.isEmpty())
      startDupThread(personName);
    else
      finishDupSearch();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void regenerateDupSearchKey()
  {
    setSearchKey(new PersonName(tfDupFirstName.getText(), tfDupLastName.getText()), tfDupSearchKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setSearchKey(PersonName name)
  {
    setSearchKey(name, tfSearchKey);
  }

  private void setSearchKey(PersonName name, TextField tf)
  {
    HDT_Person personToUse = person, dupPerson = curDupPerson();

    if (((personToUse == null) && rbMerge.isSelected()) || ((tf == tfDupSearchKey) && (dupPerson != null)))
      personToUse = dupPerson;

    StringBuilder sb = new StringBuilder();
    HDT_Person.makeSearchKey(name, personToUse, sb);
    tf.setText(sb.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopDupThread()
  {
    if ((task == null) || (task.isRunning() == false)) return;

    task.cancelAndWait();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void startDupThread(PersonName personName)
  {
    stopDupThread();

    lblStatus.setText("Searching for duplicates...");
    lblStatus.setVisible(true);
    progressIndicator.setVisible(true);

    task = matcher.createDupCheckTask(new PersonForDupCheck(origAuthor, personName), this::finishDupSearch);

    progressIndicator.progressProperty().bind(task.progressProperty());

    task.startWithNewThread();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void finishDupSearch()
  {
    progressIndicator.progressProperty().unbind();
    progressIndicator.setProgress(1.0);

    lblStatus.setText(matcher.numMatches() + " potential duplicate(s) found.");
    lblStatus.setVisible(true);

    tabPane.getSelectionModel().getSelectedItem().setContent(null);

    noTabUpdate = true;

    while (tabPane.getTabs().size() > 1)
      tabPane.getTabs().remove(1);

    noTabUpdate = false;

    tabPane.getTabs().get(0).setContent(apDup);

    if (matcher.isEmpty() == false)
    {
      tabPane.getTabs().get(0).setText("Potential dup. #1");

      int numTabs = Math.min(20, matcher.numMatches()); // prevent large number of tabs from being created

      for (int ndx = 1; ndx < numTabs; ndx++)
        tabPane.getTabs().add(new Tab("Potential dup. #" + (ndx + 1)));

      if (tabPane.isVisible() == false) // Expand dialog vertically to reveal duplicate author tabs
      {
        double height = scalePropertyValueForDPI(419),
               diff = stage.getHeight() - rootPane.getHeight();

        setHeights(rootPane, height);
        setHeights(stage   , height + diff);

        tabPane.setVisible(true);

        if (person == null)
          rbCreateNoMerge.setText("Create Record Without Merging");

        rbCreateNoMerge.setSelected(false);
        rbAddNoCreate  .setSelected(false);
      }
    }
    else
      tabPane.getTabs().get(0).setText("No potential duplicates found.");

    updateCurrentTab();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateRadioButtons()
  {
    boolean noMatches  = matcher.isEmpty(),
            notMerging = rbCreateNoMerge.isSelected() || rbAddNoCreate.isSelected() || noMatches;

    if (notMerging)
    {
      rbUseDupName.setSelected(false);
      rbUseName   .setSelected(false);
    }

    if (noMatches) rbMerge.setSelected(false);

    disableAllIff(notMerging, rbUseName, rbUseDupName);

    disableAllIff(noMatches, rbMerge, tfDupFirstName, tfDupLastName, tfDupSearchKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateCurrentTab()
  {
    tfDupSearchKey.setText("");

    lblDupTitle.setText("");
    lblDupType .setText("");
    lblDupYear .setText("");

    if (matcher.isEmpty())
    {
      tfDupFirstName.setText("");
      tfDupLastName .setText("");
    }
    else
    {
      RecordAuthor author = curDupAuthor();

      tfDupFirstName.setText(author.firstName());
      tfDupLastName .setText(author.lastName());

      nullSwitch(author.getPerson(), authPerson -> tfDupSearchKey.setText(authPerson.getSearchKey()));

      nullSwitch(author.getWork(), work ->
      {
        lblDupTitle.setText(work.name());
        lblDupType .setText(nullSwitch(work.workType.get(), "", HDT_WorkType::name));
        lblDupYear .setText(work.getYearStr());
      });

      if (tfDupSearchKey.getText().isBlank())
        regenerateDupSearchKey();
    }

    updateRadioButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML @Override protected void btnCancelClick()
  {
    stopDupThread();
    super.btnCancelClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (grpAction.getSelectedToggle() == null)
      return falseWithErrorPopup("Select which action to take.");

    if (rbAddNoCreate.isSelected())
    {
      // If there is no person record and one isn't being created, updating the
      // author must be done by the caller.

      stopDupThread();
      return true;
    }

    RecordAuthor dupAuthor = curDupAuthor();
    HDT_Person dupPerson = curDupPerson();

    if (rbMerge.isSelected())
    {
      if ((rbUseDupName.isSelected() == false) && (rbUseName.isSelected() == false))
        return falseWithErrorPopup("Select which name/search key should be used.");

      if (dupPerson != null)
      {
        if (person == null)
          person = dupPerson;
        else
        {
          warningPopup("Unable to merge automatically; a person record already exists for both authors. You will need to merge them manually.");
          rbCreateNoMerge.setSelected(true);
          return false;
        }
      }
    }

    boolean deletePerson = person == null;

    if (deletePerson)
      person = db.createNewBlankRecord(hdtPerson);

    String searchKey = rbUseDupName.isSelected() ? tfDupSearchKey.getText() : tfSearchKey.getText();

    try
    {
      person.setSearchKey(searchKey);
    }
    catch (SearchKeyException e)
    {
      if (deletePerson)
      {
        db.deleteRecord(person);
        person = null;
      }

      return e instanceof SearchKeyTooShortException ?
        falseWithErrorPopup("Unable to modify record. Search key must be at least 3 characters: " + e.getKey(), tfSearchKey)
      :
        falseWithErrorPopup("Unable to modify record. Search key already exists: " + e.getKey(), tfSearchKey);
    }

    if (rbMerge.isSelected())
    {
      if (dupPerson == null)
        dupAuthor.getWork().getAuthors().setAuthorRecord(dupAuthor, person);

      if ((origAuthor.getPerson() == null) && (origAuthor.getWork() != null))
      {
        WorkAuthors authors = origAuthor.getWork().getAuthors();

        if (authors.asCollection().contains(origAuthor))
          authors.setAuthorRecord(origAuthor, person);
      }
    }

    person.setName(rbUseDupName.isSelected() ?
      new PersonName(tfDupFirstName.getText(), tfDupLastName.getText())
    :
      new PersonName(tfFirstName.getText(), tfLastName.getText()));

    stopDupThread();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
