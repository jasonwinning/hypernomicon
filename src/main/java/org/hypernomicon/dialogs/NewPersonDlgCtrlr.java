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
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Person.PotentialKeySet;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import static java.util.Collections.*;

import java.util.*;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

import org.hypernomicon.view.MainCtrlr;

//---------------------------------------------------------------------------

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

  private final Author origAuthor;
  private final HDT_Work destWork;
  private final ArrayList<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();

  private HDT_Person person;
  private boolean alreadyChangingName = false, noTabUpdate = false;
  private HyperTask task;
  private ArrayList<Author> matchedAuthors;

  public HDT_Person getPerson()     { return person; }
  public PersonName getName()       { return new PersonName(tfFirstName.getText(), tfLastName.getText()); }
  public String getNameLastFirst()  { return getName().getLastFirst(); }
  private int numMatches()          { return nullSwitch(matchedAuthors, 0, List::size); }
  private Author curDupAuthor()     { return numMatches() == 0 ? null : matchedAuthors.get(tabPane.getSelectionModel().getSelectedIndex()); }
  private HDT_Person curDupPerson() { return nullSwitch(curDupAuthor(), null, Author::getPerson); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public NewPersonDlgCtrlr(boolean mustCreate, String name, Author origAuthor)
  {
    this("Add a New Person to the Database", name, null, null, mustCreate, null, origAuthor, new ArrayList<>(), null);
  }

  public NewPersonDlgCtrlr(boolean mustCreate, String name, Author origAuthor, HDT_Work destWork)
  {
    this("Add a New Person to the Database", name, null, null, mustCreate, null, origAuthor, new ArrayList<>(), destWork);
  }

  public NewPersonDlgCtrlr(PersonName personName, String searchKey, boolean mustCreate, HDT_Person person, Author origAuthor, ArrayList<Author> matchedAuthors)
  {
    this("Potential Duplicate(s)", null, personName, searchKey, mustCreate, person, origAuthor, matchedAuthors, null);
  }

  private NewPersonDlgCtrlr(String title, String name, PersonName personName, String searchKey, boolean mustCreate, HDT_Person person, Author origAuthor,
                            ArrayList<Author> matchedAuthors, HDT_Work destWork)
  {
    super("NewPersonDlg", title, true);

    this.matchedAuthors = matchedAuthors;
    this.person = person;
    this.origAuthor = origAuthor;
    this.destWork = destWork;

    rbCreateNoMerge.setText(person == null ? "Create Person Record" : "Don't Merge");

    if ((person == null) && (origAuthor != null))
      person = origAuthor.getPerson();

    if (person != null) mustCreate = true;

    matchedAuthorsList.add(matchedAuthors);

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

    if (mustCreate)
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

    if (name == null)
    {
      tfFirstName.setText(personName.getFirst());
      tfLastName.setText(personName.getLast());

      if (searchKey == null)
        setSearchKey(personName);
      else
        tfSearchKey.setText(searchKey);
    }
    else
    {
      personName = new PersonName(name);

      tfFirstName.setText(personName.getFirst());
      tfLastName.setText(personName.getLast());
      setSearchKey(personName);
    }

    alreadyChangingName = false;

    if (matchedAuthors.size() > 0)
      finishDupSearch();
    else
      startDupThread(personName);
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

  public static class PersonForDupCheck
  {
    private final Author author;
    private final PersonName name;
    private final PotentialKeySet keySet, keySetNoNicknames;
    private final String fullLCNameEngChar;

    public PersonForDupCheck(PersonName name, Author author) { this(author, name, convertToEnglishChars(name.getFull())); }
    public PersonForDupCheck(HDT_Person person)              { this(new Author(person)); }
    public PersonForDupCheck(Author author)                  { this(author, author.getName(), author.fullName(true)); }

  //---------------------------------------------------------------------------

    private PersonForDupCheck(Author author, PersonName name, String newFullNameEngChar)
    {
      this.author = author;
      this.name = name;

      keySet = HDT_Person.makeSearchKeySet(name, true, true, false, false);
      keySetNoNicknames = HDT_Person.makeSearchKeySet(name, true, true, true, false);

      newFullNameEngChar = removeAllParentheticals(newFullNameEngChar.toLowerCase());

      while (newFullNameEngChar.contains("  "))
        newFullNameEngChar = newFullNameEngChar.replaceAll("  ", " ");

      fullLCNameEngChar = newFullNameEngChar.strip().replaceAll("[.,;]", "");
    }

  //---------------------------------------------------------------------------

    public HDT_Person getPerson()         { return nullSwitch(author, null, Author::getPerson); }
    public Author getAuthor()             { return author; }
    public PersonName getName()           { return name; }
    public boolean startsWith(String str) { return keySetNoNicknames.startsWith(str.replaceAll("[.,;]", "")); }

    public boolean matches(PersonForDupCheck person2)
    {
      return fullLCNameEngChar.equals(person2.fullLCNameEngChar) ||
             keySetNoNicknames.isSubsetOf(person2.keySet)        ||
             person2.keySetNoNicknames.isSubsetOf(keySet);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static LinkedList<PersonForDupCheck> createListForDupCheck()
  {
    LinkedList<PersonForDupCheck> list = new LinkedList<>();
    Set<HDT_Person> persons = new HashSet<>();

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

    db.persons.stream().filter(person -> person.works.isEmpty()).map(PersonForDupCheck::new).forEachOrdered(personForDupCheck ->
    {
      if (personForDupCheck.fullLCNameEngChar.length() > 0)
        list.add(personForDupCheck);
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void doDupCheck(PersonForDupCheck person1, LinkedList<PersonForDupCheck> list, List<Author> matchedAuthors, HyperTask task) throws CancelledTaskException
  {
    if (person1.fullLCNameEngChar.isEmpty()) return;

    HDT_Work work1 = nullSwitch(person1.author, null, Author::getWork);

    for (PersonForDupCheck person2 : list)
    {
      task.incrementAndUpdateProgress(10);

      if (nullSwitch(person1.author     , false, author1    -> author1    == person2.author     )) continue;
      if (nullSwitch(person1.getPerson(), false, personRec1 -> personRec1 == person2.getPerson())) continue;

      boolean isMatch = false;

      if (person1.matches(person2))
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

      if (isMatch)
        matchedAuthors.add(person2.author);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HyperTask createDupCheckTask(PersonName name, Author queryAuthor, ArrayList<ArrayList<Author>> matchedAuthorsList, Runnable finishHndlr)
  {
    // The list passed into the second parameter needs to be able to contain null values
    return createDupCheckTask(singletonList(name), singletonList(queryAuthor), matchedAuthorsList, finishHndlr);
  }

  public static HyperTask createDupCheckTask(List<PersonName> nameList, List<Author> queryAuthors, List<ArrayList<Author>> matchedAuthorsList, Runnable finishHndlr)
  {
    return new HyperTask("CheckForDupAuthors", "Checking for duplicates...") { @Override protected void call() throws CancelledTaskException
    {
      matchedAuthorsList.clear();

      LinkedList<PersonForDupCheck> list = createListForDupCheck();

      for (int ndx = 0; ndx < nameList.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = new ArrayList<>();
        matchedAuthorsList.add(matchedAuthors);
        PersonForDupCheck person = new PersonForDupCheck(nameList.get(ndx), queryAuthors.get(ndx));

        completedCount = ((long) ndx) * ((long) (list.size()));
        totalCount = ((long) nameList.size()) * ((long) list.size());

        doDupCheck(person, list, matchedAuthors, this);
      }

      if (finishHndlr != null) runInFXThread(finishHndlr);
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

    // Next, make sure it doesn't find "duplicate" authors in the same work we are currently populating

    Author author = (destWork != null) && ((origAuthor == null) || (origAuthor.getWork() == null)) ?
      new Author(destWork, personName, false, false, Ternary.Unset)
    :
      origAuthor;

    task = createDupCheckTask(personName, author, matchedAuthorsList, this::finishDupSearch);

    progressIndicator.progressProperty().bind(task.progressProperty());

    task.startWithNewThread();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void finishDupSearch()
  {
    progressIndicator.progressProperty().unbind();
    progressIndicator.setProgress(1.0);

    matchedAuthors = matchedAuthorsList.size() > 0 ? matchedAuthorsList.get(0) : new ArrayList<>();

    if (origAuthor != null)
      matchedAuthors.removeIf(origAuthor::equals);

    lblStatus.setText(matchedAuthors.size() + " potential duplicate(s) found.");
    lblStatus.setVisible(true);

    tabPane.getSelectionModel().getSelectedItem().setContent(null);

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

      if ((origAuthor != null) && (origAuthor.getPerson() == null))
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
