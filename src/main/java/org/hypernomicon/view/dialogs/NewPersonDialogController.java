/*
 * Copyright 2015-2018 Jason Winning
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
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Person.PotentialKeySet;
import org.hypernomicon.model.records.HDT_Work;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.hdtPerson;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.function.UnaryOperator;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.layout.AnchorPane;

public class NewPersonDialogController extends HyperDialog
{
  @FXML private TextField tfLastName;
  @FXML private TextField tfFirstName;
  @FXML private TextField tfSearchKey;
  @FXML private Button btnOK;
  @FXML private Button btnUse;
  @FXML private Button btnCancel;
  @FXML private Button btnMerge;
  @FXML private Button btnDontMerge;
  @FXML private Button btnMergeCancel;
  @FXML public CheckBox chkCreate;
  @FXML private Label lblDupName;
  @FXML private Label lblDupType;
  @FXML private Label lblDupTitle;
  @FXML private Label lblDupYear;
  @FXML private Label lblStatus;
  @FXML private ProgressIndicator progressIndicator;
  @FXML private TabPane tabPane;
  @FXML private AnchorPane apDup;
  
  private HDT_Person person = null;
  private Author origAuthor = null;
  private boolean alreadyChangingName = false, noTabUpdate = false;
  private HyperTask task;
  private Thread thread;
  private List<Author> matchedAuthors = null;
  private ArrayList<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();

  public HDT_Person getPerson()    { return person; }
  public PersonName getName()      { return new PersonName(tfFirstName.getText(), tfLastName.getText()); }
  public String getNameLastFirst() { return getName().getLastFirst(); }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  public static NewPersonDialogController create(boolean mustCreate, String name, Author origAuthor)
  {
    NewPersonDialogController npd = HyperDialog.create("NewPersonDialog.fxml", "Add a New Person to the Database", true);
    npd.init(name, null, null, mustCreate, null, origAuthor, new ArrayList<>());
    return npd;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public static NewPersonDialogController create(PersonName personName, String searchKey, boolean mustCreate, HDT_Person person, Author origAuthor, ArrayList<Author> matchedAuthors)
  {
    NewPersonDialogController npd = HyperDialog.create("NewPersonDialog.fxml", "Potential Duplicate(s)", true);
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
    
    if ((person == null) && (origAuthor != null))
      person = origAuthor.getPerson();
    
    if (person != null) mustCreate = true;
    
    matchedAuthorsList.add(matchedAuthors);
       
    lblStatus.setVisible(false);
    progressIndicator.setVisible(false);
    tabPane.setVisible(false);
    
    btnDontMerge.setOnAction(event -> btnOkClick());
    btnMerge.setOnAction(event -> btnMergeClick());
    btnUse.setOnAction(event -> btnUseClick());
    
    tabPane.getSelectionModel().selectedIndexProperty().addListener((observable, oldValue, newValue) ->
    {
      if (noTabUpdate) return;
            
      Tab.class.cast(tabPane.getTabs().get(oldValue.intValue())).setContent(null);
      Tab.class.cast(tabPane.getTabs().get(newValue.intValue())).setContent(apDup);
      
      updateCurrentTab();
    });
    
    tfFirstName.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (alreadyChangingName) return;
      setSearchKey(new PersonName(newValue, tfLastName.getText()));
      startDupThread(new PersonName(newValue, tfLastName.getText()));
    });
    
    if (mustCreate)
      chkCreate.setDisable(true);
    
    tfSearchKey.disableProperty().bind(chkCreate.selectedProperty().not());
    
    chkCreate.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == false)
        btnMerge.setDisable(true);
      else
        btnMerge.setDisable(matchedAuthors.size() == 0);
    });
    
    UnaryOperator<TextFormatter.Change> filter = (change) ->
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
    };
         
    tfLastName.setTextFormatter(new TextFormatter<>(filter));
    
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
    StringBuilder sb = new StringBuilder();
    HDT_Person.makeSearchKey(name, person, sb);
    tfSearchKey.setText(sb.toString());
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override protected boolean isValid()
  {
    if (chkCreate.isSelected() == false)
    {
      stopDupThread();
      return true;
    }

    boolean deletePerson = (person == null);
    
    if (person == null)
      person = db.createNewBlankRecord(hdtPerson);
    
    try 
    {
      person.setSearchKey(tfSearchKey.getText());
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
    
    person.setName(new PersonName(tfFirstName.getText(), tfLastName.getText()));
    
    stopDupThread();
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void expand()
  {
    double height = 323.0 * displayScale;
    double diff = dialogStage.getHeight() - mainPane.getHeight();
    
    mainPane.setMinHeight(height);    
    mainPane.setMaxHeight(height);
    mainPane.setPrefHeight(height);
    
    dialogStage.setMinHeight(height + diff);
    dialogStage.setMaxHeight(height + diff);
    dialogStage.setHeight(height + diff);
           
    tabPane.setVisible(true);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void stopDupThread()
  {
    if (task == null) return;
    
    if (task.isRunning())
    {
      task.cancel();
      try { thread.join(); } catch (Exception e) { noOp(); }
    }    
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static class PersonForDupCheck
  {
    public PersonForDupCheck(Author author)
    {
      this.author = author;
      keySet = HDT_Person.makeSearchKeySet(author.getName(), true, false, true);
      
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
      
      keySet = HDT_Person.makeSearchKeySet(name, true, false, true);
      
      fullLCNameEngChar = convertToEnglishChars(String.valueOf(name.getFull())).toLowerCase();
      while (fullLCNameEngChar.contains("("))
        fullLCNameEngChar = removeFirstParenthetical(fullLCNameEngChar);
      
      fullLCNameEngChar = ultraTrim(fullLCNameEngChar);
    }

    public HDT_Person getPerson()    { return author == null ? null : author.getPerson(); }    
    public Author getAuthor()        { return author; }    
    public PersonName getName()      { return name; }      
    
    private Author author;
    private PotentialKeySet keySet;
    private String fullLCNameEngChar;
    private PersonName name;    
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static LinkedList<PersonForDupCheck> createListForDupCheck()
  {
    LinkedList<PersonForDupCheck> list = new LinkedList<>();
    HashSet<HDT_Person> persons = new HashSet<>();
    PersonForDupCheck personForDupCheck;
    
    for (HDT_Work work : db.works)
    {
      Authors authors = work.getAuthors();
                
      if (work.getAuthors().size() == 0)
        continue;
      
      for (Author author : authors)
      {
        HDT_Person person = author.getPerson();
        
        if (person != null)
        {
          if (persons.contains(person))
            continue;
          
          persons.add(person);
        }
        
        personForDupCheck = new PersonForDupCheck(author);
        
        if (personForDupCheck.fullLCNameEngChar.length() > 0)
          list.add(personForDupCheck);        
      }
    }
    
    for (HDT_Person person : db.persons)
    {
      if (person.works.isEmpty())
      {
        personForDupCheck = new PersonForDupCheck(new Author(person));
        
        if (personForDupCheck.fullLCNameEngChar.length() > 0)
          list.add(personForDupCheck);        
      }
    }
    
    return list;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public static void doDupCheck(PersonForDupCheck person1, LinkedList<PersonForDupCheck> list, ArrayList<Author> matchedAuthors, HyperTask task, int ctr, int total) throws TerminateTaskException
  {
    HashSet<HDT_Person> matchedPersons = new HashSet<>();
    
    for (PersonForDupCheck person2 : list)
    {        
      if (person1.author == person2.author)                continue;
      else if (person1.getPerson() == person2.getPerson()) continue;
      
      boolean isMatch = false;
      
      if      (person1.keySet.isSubsetOf(person2.keySet))                   isMatch = true;
      else if (person2.keySet.isSubsetOf(person1.keySet))                   isMatch = true;
      else if (person1.fullLCNameEngChar.equals(person2.fullLCNameEngChar)) isMatch = true;
      
      if (isMatch)
      {
        matchedAuthors.add(person2.author);
        HDT_Person personRecord = person2.author.getPerson();
        if (personRecord != null)
          matchedPersons.add(personRecord);       
      }
      
      if (task.isCancelled()) throw new TerminateTaskException();
      
      if ((ctr % 10) == 0) task.updateProgress(ctr, total);
      
      ctr++;
    }
    
    // Now make sure it is not an exact match (in terms of auto-generated search key) with an existing person record
    
    HDT_Person otherPerson = HDT_Person.lookUpByName(person1.getName());
    
    if ((otherPerson != person1.getPerson()) && (otherPerson != null))
      if (matchedPersons.contains(otherPerson) == false)
      {
        matchedAuthors.add(new Author(otherPerson));
        matchedPersons.add(otherPerson);
      }
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public static HyperTask createDupCheckTask(List<PersonName> nameList, Author queryAuthor, HDT_Person queryPerson, List<ArrayList<Author>> matchedAuthorsList, Runnable finishHndlr)
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
        
        PersonForDupCheck person = new PersonForDupCheck(nameList.get(ndx), queryAuthor, queryPerson);
        
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
      
    task = createDupCheckTask(Collections.singletonList(personName), 
                              origAuthor, person, matchedAuthorsList, () -> finishDupSearch());
        
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
    
    matchedAuthors = matchedAuthorsList.get(0);
    
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
    
    if (matchedAuthors.size() > 0)
    {     
      tabPane.getTabs().get(0).setText("Potential dup. #1");
      
      for (int ndx = 1; ndx < matchedAuthors.size(); ndx++)
      {
        Tab tab;
        
        if (ndx > 0)
        {
          tab = new Tab("Potential dup. #" + String.valueOf(ndx + 1));
          tabPane.getTabs().add(tab);
        }
      }
      
      btnMerge.setDisable(false);
      btnDontMerge.setDisable(false);
      btnMergeCancel.setDisable(false);
      
      btnUse.setDisable(false);
      
      btnOK.setDisable(true);
      btnCancel.setDisable(true);
      
      btnOK.setDefaultButton(false);
      btnCancel.setCancelButton(false);
      
      btnMerge.setDefaultButton(true);
      btnMergeCancel.setCancelButton(true);
      
      updateCurrentTab();
      
      if (tabPane.isVisible() == false)
        expand();
    }
    else
    {
      tabPane.getTabs().get(0).setText("No potential duplicates found.");
      
      lblDupName.setText("");
      lblDupTitle.setText("");
      lblDupType.setText("");
      lblDupYear.setText("");
      
      btnUse.setDisable(true);
      
      btnMerge.setDisable(true);
      btnDontMerge.setDisable(true);
      btnMergeCancel.setDisable(true);
      
      btnOK.setDisable(false);
      btnCancel.setDisable(false);

      btnMerge.setDefaultButton(false);
      btnMergeCancel.setCancelButton(false);

      btnOK.setDefaultButton(true);
      btnCancel.setCancelButton(true);
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void updateCurrentTab()
  {
    int ndx = tabPane.getSelectionModel().getSelectedIndex();
    
    Author author = matchedAuthors.get(ndx);
    
    lblDupName.setText(author.getNameLastFirst());
    
    HDT_Work work = author.getWork();
    if (work != null)
    {
      lblDupTitle.setText(work.name());
      lblDupType.setText(work.workType.get().name());
      lblDupYear.setText(work.getYear());
    }
    else
    {
      lblDupTitle.setText("");
      lblDupType.setText("");
      lblDupYear.setText("");
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @FXML @Override protected void btnCancelClick()
  {
    okClicked = false;
    stopDupThread();
    dialogStage.close();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void btnUseClick()
  {    
    Author author = matchedAuthors.get(tabPane.getSelectionModel().getSelectedIndex());
   
    if (getNameLastFirst().equals(author.getNameLastFirst()))
      return;
    
    alreadyChangingName = true;
    tfFirstName.setText(author.getFirstName());
    alreadyChangingName = false;
    
    tfLastName.setText(author.getLastName());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void btnMergeClick()
  {
    Author author = matchedAuthors.get(tabPane.getSelectionModel().getSelectedIndex());
    
    if ((author.getPerson() != null) && chkCreate.isSelected() && (chkCreate.isDisabled() == false))
      chkCreate.setSelected(false);
      
    if (isValid() == false) return; // if chkCreate is selected, isValid sets 'person' equal to a new person record. Otherwise, it does nothing.
    
    if (author.getPerson() != null)
    {
      if (person == null)
        person = author.getPerson();
      else
      {
        messageDialog("Unable to merge automatically; a person record already exists for both authors. You will need to merge them manually.", mtWarning);
        okClicked = true;
        dialogStage.close();
        return;
      }
    }
    
    if (author.getPerson() == null)
      author.getWork().getAuthors().setAuthorRecord(author, person);
    
    if (origAuthor != null)
      if (origAuthor.getPerson() == null)
        origAuthor.getWork().getAuthors().setAuthorRecord(origAuthor, person);
    
    person.setName(new PersonName(tfFirstName.getText(), tfLastName.getText()));
    
    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
}
