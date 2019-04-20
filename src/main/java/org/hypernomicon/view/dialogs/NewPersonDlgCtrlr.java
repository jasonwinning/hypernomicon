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

package org.hypernomicon.view.dialogs;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Person.PotentialKeySet;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.RadioButton;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;

public class NewPersonDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apDup;
  @FXML private Button btnCancel, btnOK;
  @FXML private Label lblDupSearchKey, lblDupTitle, lblDupType, lblDupYear, lblSearchKey, lblStatus;
  @FXML private ProgressIndicator progressIndicator;
  @FXML private RadioButton rbAddNoCreate, rbCreateNoMerge, rbMerge, rbUseDupName, rbUseName;
  @FXML private TabPane tabPane;
  @FXML private TextField tfDupFirstName, tfDupLastName, tfDupSearchKey, tfFirstName, tfLastName, tfSearchKey;
  @FXML private ToggleGroup grpAction;

  private HDT_Person person = null;
  private Author origAuthor = null;
  private boolean alreadyChangingName = false, noTabUpdate = false;
  private HyperTask task;
  private Thread thread;
  private List<Author> matchedAuthors = null;
  private final ArrayList<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();

  public HDT_Person getPerson()     { return person; }
  public PersonName getName()       { return new PersonName(tfFirstName.getText(), tfLastName.getText()); }
  public String getNameLastFirst()  { return getName().getLastFirst(); }
  private int numMatches()          { return nullSwitch(matchedAuthors, 0, List::size); }
  private Author curDupAuthor()     { return numMatches() == 0 ? null : matchedAuthors.get(tabPane.getSelectionModel().getSelectedIndex()); }
  private HDT_Person curDupPerson() { return nullSwitch(curDupAuthor(), null, Author::getPerson); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewPersonDlgCtrlr create(boolean mustCreate, String name, Author origAuthor)
  {
    NewPersonDlgCtrlr npd = HyperDlg.create("NewPersonDlg.fxml", "Add a New Person to the Database", true);
    npd.init(name, null, null, mustCreate, null, origAuthor, new ArrayList<>());
    return npd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewPersonDlgCtrlr create(PersonName personName, String searchKey, boolean mustCreate, HDT_Person person, Author origAuthor, ArrayList<Author> matchedAuthors)
  {
    NewPersonDlgCtrlr npd = HyperDlg.create("NewPersonDlg.fxml", "Potential Duplicate(s)", true);
    npd.init(null, personName, searchKey, mustCreate, person, origAuthor, matchedAuthors);
    return npd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(String name, PersonName personName, String searchKey, boolean mustCreate, HDT_Person person, Author origAuthor, ArrayList<Author> matchedAuthors)
  {
    this.matchedAuthors = matchedAuthors;
    this.person = person;
    this.origAuthor = origAuthor;

    rbCreateNoMerge.setText(person == null ? "Create Person Record" : "Don't Merge");

    if ((person == null) && (origAuthor != null))
      person = origAuthor.getPerson();

    if (person != null) mustCreate = true;

    matchedAuthorsList.add(matchedAuthors);

    setAllVisible(false, lblStatus, progressIndicator, tabPane);

    tabPane.getSelectionModel().selectedIndexProperty().addListener((ob, oldValue, newValue) ->
    {
      if (noTabUpdate) return;

      Tab.class.cast(tabPane.getTabs().get(oldValue.intValue())).setContent(null);
      Tab.class.cast(tabPane.getTabs().get(newValue.intValue())).setContent(apDup);

      updateCurrentTab();
    });

    lblSearchKey.setTooltip(new Tooltip("Regenerate search key"));
    lblDupSearchKey.setTooltip(new Tooltip("Regenerate search key"));

    lblSearchKey.setOnMouseClicked(event -> setSearchKey(new PersonName(tfFirstName.getText(), tfLastName.getText())));

    lblDupSearchKey.setOnMouseClicked(event -> setSearchKey(new PersonName(tfDupFirstName.getText(), tfDupLastName.getText()), tfDupSearchKey));

    tfFirstName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (alreadyChangingName) return;
      setSearchKey(new PersonName(newValue, tfLastName.getText()));
      startDupThread(new PersonName(newValue, tfLastName.getText()));
    });

    if (mustCreate)
    {
      rbAddNoCreate.setDisable(true);
      rbCreateNoMerge.setSelected(true);
    }

    grpAction.selectedToggleProperty().addListener((ob, ov, nv) -> updateRadioButtons());

    tfSearchKey.disableProperty().bind(rbAddNoCreate.selectedProperty());

    tfLastName.setTextFormatter(new TextFormatter<>(change ->
    {
      if (alreadyChangingName) return change;

      if (change.getText().length() > 1)
      {
        if ((tfFirstName.getText().length() == 0) && (change.getControlText().length() == 0))
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

    dialogStage.setOnHidden(event -> stopDupThread());

    if (name == null)
    {
      alreadyChangingName = true;

      tfFirstName.setText(personName.getFirst());
      tfLastName.setText(personName.getLast());

      if (searchKey == null)
        setSearchKey(personName);
      else
        tfSearchKey.setText(searchKey);

      alreadyChangingName = false;
    }
    else
    {
      alreadyChangingName = true;

      personName = new PersonName(name);

      tfFirstName.setText(personName.getFirst());
      tfLastName.setText(personName.getLast());
      setSearchKey(personName);

      alreadyChangingName = false;
    }

    if (matchedAuthors.size() > 0)
      finishDupSearch();
    else
      startDupThread(personName);
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

    if ((personToUse == null) || ((tf == tfDupSearchKey) && (dupPerson != null)))
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

    task.cancel();
    try { thread.join(); } catch (Exception e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class PersonForDupCheck
  {
    public PersonForDupCheck(Author author)
    {
      this.author = author;
      keySet = HDT_Person.makeSearchKeySet(author.getName(), true, true, false);
      keySetNoNicknames = HDT_Person.makeSearchKeySet(author.getName(), true, true, true);

      fullLCNameEngChar = author.getFullName(true).toLowerCase();
      while (fullLCNameEngChar.contains("("))
        fullLCNameEngChar = removeFirstParenthetical(fullLCNameEngChar);

      fullLCNameEngChar = ultraTrim(fullLCNameEngChar);

      name = author.getName();
    }

    public PersonForDupCheck(PersonName name, Author author, HDT_Person person)
    {
      if ((author == null) && (person != null))
        author = new Author(person);

      this.author = author;
      this.name = name;

      keySet = HDT_Person.makeSearchKeySet(name, true, true, false);
      keySetNoNicknames = HDT_Person.makeSearchKeySet(name, true, true, true);

      fullLCNameEngChar = convertToEnglishChars(String.valueOf(name.getFull())).toLowerCase();
      while (fullLCNameEngChar.contains("("))
        fullLCNameEngChar = removeFirstParenthetical(fullLCNameEngChar);

      fullLCNameEngChar = ultraTrim(fullLCNameEngChar);
    }

    public HDT_Person getPerson() { return nullSwitch(author, null, Author::getPerson); }
    public Author getAuthor()     { return author; }
    public PersonName getName()   { return name; }

    private Author author;
    private PotentialKeySet keySet, keySetNoNicknames;
    private String fullLCNameEngChar;
    private PersonName name;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static LinkedList<PersonForDupCheck> createListForDupCheck()
  {
    LinkedList<PersonForDupCheck> list = new LinkedList<>();
    HashSet<HDT_Person> persons = new HashSet<>();

    db.works.forEach(work -> work.getAuthors().forEach(author ->
    {
      HDT_Person person = author.getPerson();

      if (person != null)
      {
        if (persons.contains(person))
          return;

        persons.add(person);
      }

      PersonForDupCheck personForDupCheck = new PersonForDupCheck(author);

      if (personForDupCheck.fullLCNameEngChar.length() > 0)
        list.add(personForDupCheck);
    }));

    db.persons.stream().filter(person -> person.works.isEmpty()).map(Author::new).map(PersonForDupCheck::new).forEach(personForDupCheck ->
    {
      if (personForDupCheck.fullLCNameEngChar.length() > 0)
        list.add(personForDupCheck);
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void doDupCheck(PersonForDupCheck person1, LinkedList<PersonForDupCheck> list, ArrayList<Author> matchedAuthors, HyperTask task, int ctr, int total) throws TerminateTaskException
  {
    if (person1.fullLCNameEngChar.length() == 0) return;

    HashSet<HDT_Person> matchedPersons = new HashSet<>();

    HDT_Work work1 = nullSwitch(person1.author, null, Author::getWork);

    for (PersonForDupCheck person2 : list)
    {
      if      (nullSwitch(person1.author     , false, author1    -> author1    == person2.author     )) continue;
      else if (nullSwitch(person1.getPerson(), false, personRec1 -> personRec1 == person2.getPerson())) continue;

      boolean isMatch = false;

      if (person1.fullLCNameEngChar.equals(person2.fullLCNameEngChar))
      {
        if (work1 != null)
        {
          Author author2 = person2.author;

          if (author2 != null)
          {
            if (work1 == author2.getWork())
              continue;

            if (nullSwitch(author2.getPerson(), false, personRec2 -> work1.getAuthors().containsPerson(personRec2)))
              continue;
          }
        }

        if (nullSwitch(person2.author     , false, author2    ->
            nullSwitch(author2.getWork()  , false, work2      ->
            nullSwitch(person1.author     , false, author1    ->
            nullSwitch(author1.getPerson(), false, personRec1 -> work2.getAuthors().containsPerson(personRec1))))))
          continue;

        isMatch = true;
      }
      else if (person1.keySetNoNicknames.isSubsetOf(person2.keySet) ||
               person2.keySetNoNicknames.isSubsetOf(person1.keySet))
        isMatch = true;

      if (isMatch)
      {
        matchedAuthors.add(person2.author);
        nullSwitch(person2.author.getPerson(), matchedPersons::add);
      }

      if (task.isCancelled()) throw new TerminateTaskException();

      if ((ctr % 10) == 0) task.updateProgress(ctr, total);

      ctr++;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HyperTask createDupCheckTask(List<PersonName> nameList, List<Author> queryAuthors, List<ArrayList<Author>> matchedAuthorsList, Runnable finishHndlr)
  {
    return new HyperTask() { @Override protected Boolean call() throws Exception
    {
      matchedAuthorsList.clear();

      updateMessage("Checking for duplicates...");

      LinkedList<PersonForDupCheck> list = createListForDupCheck();

      for (int ndx = 0; ndx < nameList.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = new ArrayList<>();
        matchedAuthorsList.add(matchedAuthors);
        Author author = queryAuthors.get(ndx);
        PersonForDupCheck person = new PersonForDupCheck(nameList.get(ndx), author, author == null ? null : author.getPerson());

        doDupCheck(person, list, matchedAuthors, this, ndx * list.size(), nameList.size() * list.size());
      }

      succeeded();

      if (finishHndlr != null) runInFXThread(finishHndlr);

      return true;
    }};
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void startDupThread(PersonName personName)
  {
    stopDupThread();

    lblStatus.setText("Searching for duplicates...");
    lblStatus.setVisible(true);
    progressIndicator.setVisible(true);

    task = createDupCheckTask(Arrays.asList(personName), Arrays.asList(origAuthor), matchedAuthorsList, this::finishDupSearch);

    task.updateProgress(0, 1);

    progressIndicator.progressProperty().bind(task.progressProperty());

    thread = new Thread(task);
    task.setThread(thread);
    thread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void finishDupSearch()
  {
    progressIndicator.progressProperty().unbind();
    progressIndicator.setProgress(1.0);

    matchedAuthors = matchedAuthorsList.size() > 0 ? matchedAuthorsList.get(0) : Collections.emptyList();

    if (origAuthor != null)
      while (matchedAuthors.contains(origAuthor))
        matchedAuthors.remove(origAuthor);

    lblStatus.setText(matchedAuthors.size() + " potential duplicate(s) found.");
    lblStatus.setVisible(true);

    Tab.class.cast(tabPane.getSelectionModel().getSelectedItem()).setContent(null);

    noTabUpdate = true;

    while (tabPane.getTabs().size() > 1)
      tabPane.getTabs().remove(1);

    noTabUpdate = false;

    tabPane.getTabs().get(0).setContent(apDup);

    if (numMatches() > 0)
    {
      tabPane.getTabs().get(0).setText("Potential dup. #1");

      int numTabs = Math.min(20, matchedAuthors.size()); // prevent large number of tabs from being created

      for (int ndx = 1; ndx < numTabs; ndx++)
        tabPane.getTabs().add(new Tab("Potential dup. #" + String.valueOf(ndx + 1)));

      if (tabPane.isVisible() == false) // Expand dialog vertically to reveal duplicate author tabs
      {
        double height = 419.0 * displayScale,
               diff = dialogStage.getHeight() - mainPane.getHeight();

         mainPane.setMinHeight(height);
         mainPane.setMaxHeight(height);
         mainPane.setPrefHeight(height);

         dialogStage.setMinHeight(height + diff);
         dialogStage.setMaxHeight(height + diff);
         dialogStage.setHeight(height + diff);

         tabPane.setVisible(true);

         if (person == null)
           rbCreateNoMerge.setText("Create Record Without Merging");

         rbCreateNoMerge.setSelected(false);
         rbAddNoCreate.setSelected(false);
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
    boolean noMatches = numMatches() == 0,
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

    if (numMatches() == 0)
    {
      tfDupFirstName.setText("");
      tfDupLastName .setText("");
    }
    else
    {
      Author author = curDupAuthor();

      tfDupFirstName.setText(author.getFirstName());
      tfDupLastName .setText(author.getLastName());

      nullSwitch(author.getPerson(), authPerson -> tfDupSearchKey.setText(authPerson.getSearchKey()));

      nullSwitch(author.getWork(), work ->
      {
        lblDupTitle.setText(work.name());
        lblDupType .setText(nullSwitch(work.workType.get(), "", HDT_WorkType::name));
        lblDupYear .setText(work.getYear());
      });
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
      return falseWithErrorMessage("Select which action to take.");

    if (rbAddNoCreate.isSelected())
    {
      stopDupThread();

      if (origAuthor != null)
      {
        HDT_Work work = origAuthor.getWork();
        PersonName newName = new PersonName(tfFirstName.getText(), tfLastName.getText());

        work.getAuthors().update(origAuthor, new Author(work, newName, origAuthor.getIsEditor(), origAuthor.getIsTrans(), origAuthor.getInFileName()));
      }

      return true;
    }

    Author dupAuthor = curDupAuthor();
    HDT_Person dupPerson = curDupPerson();

    if (rbMerge.isSelected())
    {
      if ((rbUseDupName.isSelected() == false) && (rbUseName.isSelected() == false))
        return falseWithErrorMessage("Select which name/search key should be used.");

      if (dupPerson != null)
      {
        if (person == null)
          person = dupPerson;
        else
        {
          messageDialog("Unable to merge automatically; a person record already exists for both authors. You will need to merge them manually.", mtWarning);
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
        db.deleteRecord(hdtPerson, person.getID());
        person = null;
      }

      if (e.getTooShort())
        messageDialog("Unable to modify record: search key must be at least 3 characters.", mtError);
      else
        messageDialog("Unable to modify record: search key already exists.", mtError);

      safeFocus(tfSearchKey);
      return false;
    }

    if (rbMerge.isSelected())
    {
      if (dupPerson == null)
        dupAuthor.getWork().getAuthors().setAuthorRecord(dupAuthor, person);

      if ((origAuthor != null) && (origAuthor.getPerson() == null))
      {
        Authors authors = origAuthor.getWork().getAuthors();

        if (authors.asCollection().contains(origAuthor))
          authors.setAuthorRecord(origAuthor, person);
      }
    }

    if (rbUseDupName.isSelected())
      person.setName(new PersonName(tfDupFirstName.getText(), tfDupLastName.getText()));
    else
      person.setName(new PersonName(tfFirstName.getText(), tfLastName.getText()));

    stopDupThread();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
