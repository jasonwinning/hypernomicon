/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.data.RISBibData;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicYear;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

public class RISTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  public void testRISImport()
  {
    List<BibAuthor> authorList     = new ArrayList<>(),
                    editorList     = new ArrayList<>(),
                    translatorList = new ArrayList<>();

    String risExample = """

      TY  - JOUR
      AU  - Green, Sara
      AU  - Şerban, Maria
      AU  - Scholl, Raphael
      AU  - Jones, Nicholaos
      AU  - Brigandt, Ingo
      AU  - Bechtel, William
      T1  - Network analyses in systems biology: new strategies for dealing with biological complexity
      JO  - Synthese
      Y1  - 2017/01

      M3  - https://doi.org/10.1007/s11229-016-1307-6

      ER  -
      """;

    RISBibData risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Network analyses in systems biology: new strategies for dealing with biological complexity", risBibData.getStr(bfTitle));
    assertEquals("Synthese", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 1, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2017)), risBibData.getDate());
    assertEquals("https://doi.org/10.1007/s11229-016-1307-6", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(6, authorList.size());
    assertEquals("Green, Sara"     , authorList.get(0).getName().getLastFirst());
    assertEquals("Şerban, Maria"   , authorList.get(1).getName().getLastFirst());
    assertEquals("Scholl, Raphael" , authorList.get(2).getName().getLastFirst());
    assertEquals("Jones, Nicholaos", authorList.get(3).getName().getLastFirst());
    assertEquals("Brigandt, Ingo"  , authorList.get(4).getName().getLastFirst());
    assertEquals("Bechtel, William", authorList.get(5).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - JOUR
      AU  - Mossio, Matteo
      AU  - Saborido, C.
      AU  - Moreno, Alvaro
      DA  - 2009/9//
      DO  - 10.1093/bjps/axp036
      IS  - 4
      PB  - University of Chicago Press
      PY  - 2009
      SP  - 813
      EP  - 841
      TI  - An Organizational Account of Biological Functions
      T2  - The British Journal for the Philosophy of Science
      UR  - https://www.journals.uchicago.edu/doi/pdf/10.1093/bjps/axp036
      VL  - 60
      ER  -
      TY  - BOOK
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("An Organizational Account of Biological Functions", risBibData.getStr(bfTitle));
    assertEquals("The British Journal for the Philosophy of Science", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 9, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2009)), risBibData.getDate());
    assertEquals("10.1093/bjps/axp036", risBibData.getStr(bfDOI));
    assertEquals("60", risBibData.getStr(bfVolume));
    assertEquals("4", risBibData.getStr(bfIssue));
    assertEquals("University of Chicago Press", risBibData.getStr(bfPublisher));
    assertEquals("813-841", risBibData.getStr(bfPages));
    assertEquals("https://www.journals.uchicago.edu/doi/pdf/10.1093/bjps/axp036", risBibData.getStr(bfURL));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(3, authorList.size());
    assertEquals("Mossio, Matteo", authorList.get(0).getName().getLastFirst());
    assertEquals("Saborido, C."  , authorList.get(1).getName().getLastFirst());
    assertEquals("Moreno, Alvaro", authorList.get(2).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - CHAP
      TI  - Disease: Definition and Objectivity
      AU  - Kaufman, Frederik
      T2  - What Is Disease?
      A2  - Humber, James M.
      A2  - Almeder, Robert F.
      CY  - Totowa, NJ
      DA  - 1997///
      PY  - 1997
      SP  - 271
      EP  - 286
      PB  - Humana
      SN  - 978-1-61737-015-1 978-1-59259-451-1
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etBookChapter, risBibData.getEntryType());
    assertEquals("Disease: Definition and Objectivity", risBibData.getStr(bfTitle));
    assertEquals("What Is Disease?", risBibData.getStr(bfContainerTitle));
    assertEquals("Totowa, NJ", risBibData.getStr(bfPubLoc));
    assertEquals("Humana", risBibData.getStr(bfPublisher));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(1997)), risBibData.getDate());
    assertEquals("271-286", risBibData.getStr(bfPages));
    assertEquals(List.of("9781617370151", "9781592594511"), risBibData.getMultiStr(bfISBNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Kaufman, Frederik" , authorList.get(0).getName().getLastFirst());

    assertEquals(2, editorList.size());
    assertEquals("Humber, James M."  , editorList.get(0).getName().getLastFirst());
    assertEquals("Almeder, Robert F.", editorList.get(1).getName().getLastFirst());

    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - CHAP
      TI  - Disease and Subjectivity
      AU  - van Hooft, Stan
      T2  - What Is Disease?
      A2  - Humber, James M.
      A2  - Almeder, Robert F.
      CY  - Totowa, NJ
      DA  - 1997///
      PY  - 1997
      SP  - 287
      EP  - 323
      PB  - Humana
      SN  - 978-1-61737-015-1 978-1-59259-451-1
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etBookChapter, risBibData.getEntryType());
    assertEquals("Disease and Subjectivity", risBibData.getStr(bfTitle));
    assertEquals("What Is Disease?", risBibData.getStr(bfContainerTitle));
    assertEquals("Totowa, NJ", risBibData.getStr(bfPubLoc));
    assertEquals("Humana", risBibData.getStr(bfPublisher));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(1997)), risBibData.getDate());
    assertEquals("287-323", risBibData.getStr(bfPages));
    assertEquals(List.of("9781617370151", "9781592594511"), risBibData.getMultiStr(bfISBNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("van Hooft, Stan" , authorList.get(0).getName().getLastFirst());

    assertEquals(2, editorList.size());
    assertEquals("Humber, James M."  , editorList.get(0).getName().getLastFirst());
    assertEquals("Almeder, Robert F.", editorList.get(1).getName().getLastFirst());

    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """
      TY  - CHAP
      TI  - A Rebuttal on Health
      AU  - Boorse, Christopher
      T2  - What Is Disease?
      A2  - Humber, James M.
      A2  - Almeder, Robert F.
      CY  - Totowa, NJ
      DA  - 1997///
      PY  - 1997
      SP  - 3
      EP  - 134
      PB  - Humana
      SN  - 978-1-61737-015-1 978-1-59259-451-1
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etBookChapter, risBibData.getEntryType());
    assertEquals("A Rebuttal on Health", risBibData.getStr(bfTitle));
    assertEquals("What Is Disease?", risBibData.getStr(bfContainerTitle));
    assertEquals("Totowa, NJ", risBibData.getStr(bfPubLoc));
    assertEquals("Humana", risBibData.getStr(bfPublisher));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(1997)), risBibData.getDate());
    assertEquals("3-134", risBibData.getStr(bfPages));
    assertEquals(List.of("9781617370151", "9781592594511"), risBibData.getMultiStr(bfISBNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Boorse, Christopher" , authorList.get(0).getName().getLastFirst());

    assertEquals(2, editorList.size());
    assertEquals("Humber, James M."  , editorList.get(0).getName().getLastFirst());
    assertEquals("Almeder, Robert F.", editorList.get(1).getName().getLastFirst());

    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """
      TY  - BOOK
      TI  - What Is Disease?
      A3  - Humber, James M.
      A3  - Almeder, Robert F.
      CY  - Totowa, NJ
      DA  - 1997///
      PY  - 1997
      PB  - Humana
      SN  - 978-1-61737-015-1 978-1-59259-451-1
      UR  - http://link.springer.com/content/pdf/10.1007/978-1-59259-451-1.pdf
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etBook, risBibData.getEntryType());
    assertEquals("What Is Disease?", risBibData.getStr(bfTitle));
    assertEquals("Totowa, NJ", risBibData.getStr(bfPubLoc));
    assertEquals("Humana", risBibData.getStr(bfPublisher));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(1997)), risBibData.getDate());
    assertEquals(List.of("9781617370151", "9781592594511"), risBibData.getMultiStr(bfISBNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(0, authorList.size());

    assertEquals(2, editorList.size());
    assertEquals("Humber, James M."  , editorList.get(0).getName().getLastFirst());
    assertEquals("Almeder, Robert F.", editorList.get(1).getName().getLastFirst());

    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """
      TY  - JOUR
      TI  - Are Plants Cognitive? A Reply to Adams
      AU  - Segundo-Ortin, Miguel
      AU  - Calvo, Paco
      T2  - Studies in History and Philosophy of Science Part A
      DA  - 2019///
      PY  - 2019
      DO  - 10.1016/j.shpsa.2018.12.001
      VL  - 73
      SP  - 64
      EP  - 71
      SN  - 0039-3681
      UR  - https://api.elsevier.com/content/article/PII:S0039368118302383?httpAccept=text/xml
      ER  -""";

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Are Plants Cognitive? A Reply to Adams", risBibData.getStr(bfTitle));
    assertEquals("Studies in History and Philosophy of Science Part A", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2019)), risBibData.getDate());
    assertEquals("10.1016/j.shpsa.2018.12.001", risBibData.getStr(bfDOI));
    assertEquals("73", risBibData.getStr(bfVolume));
    assertEquals("", risBibData.getStr(bfIssue));
    assertEquals("64-71", risBibData.getStr(bfPages));
    assertEquals("https://api.elsevier.com/content/article/PII:S0039368118302383?httpAccept=text/xml", risBibData.getStr(bfURL));
    assertEquals(List.of("0039-3681"), risBibData.getMultiStr(bfISSNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(2, authorList.size());
    assertEquals("Segundo-Ortin, Miguel", authorList.get(0).getName().getLastFirst());
    assertEquals("Calvo, Paco"  , authorList.get(1).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - JOUR
      TI  - Moving and Sensing Without Input and Output: Early Nervous Systems and the Origins of the Animal Sensorimotor Organization
      AU  - Keizer, Fred
      T2  - Biology & Philosophy
      DA  - 2015///
      PY  - 2015
      DO  - 10.1007/s10539-015-9483-1
      VL  - 30
      IS  - 3
      SP  - 311
      EP  - 331
      SN  - 0169-3867 1572-8404
      UR  - http://link.springer.com/content/pdf/10.1007/s10539-015-9483-1.pdf
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Moving and Sensing Without Input and Output: Early Nervous Systems and the Origins of the Animal Sensorimotor Organization", risBibData.getStr(bfTitle));
    assertEquals("Biology & Philosophy", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2015)), risBibData.getDate());
    assertEquals("10.1007/s10539-015-9483-1", risBibData.getStr(bfDOI));
    assertEquals("30", risBibData.getStr(bfVolume));
    assertEquals("3", risBibData.getStr(bfIssue));
    assertEquals("311-331", risBibData.getStr(bfPages));
    assertEquals("http://link.springer.com/content/pdf/10.1007/s10539-015-9483-1.pdf", risBibData.getStr(bfURL));
    assertEquals(List.of("0169-3867", "1572-8404"), risBibData.getMultiStr(bfISSNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Keizer, Fred", authorList.get(0).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - JOUR
      TI  - Cognition Wars
      AU  - Adams, Fred
      T2  - Studies in History and Philosophy of Science Part A
      DA  - 2018///
      PY  - 2018
      DO  - 10.1016/j.shpsa.2017.11.007
      VL  - 68
      SP  - 20
      EP  - 30
      SN  - 0039-3681
      UR  - https://api.elsevier.com/content/article/PII:S0039368117302741?httpAccept=text/xml
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Cognition Wars", risBibData.getStr(bfTitle));
    assertEquals("Studies in History and Philosophy of Science Part A", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2018)), risBibData.getDate());
    assertEquals("10.1016/j.shpsa.2017.11.007", risBibData.getStr(bfDOI));
    assertEquals("68", risBibData.getStr(bfVolume));
    assertEquals("", risBibData.getStr(bfIssue));
    assertEquals("20-30", risBibData.getStr(bfPages));
    assertEquals("https://api.elsevier.com/content/article/PII:S0039368117302741?httpAccept=text/xml", risBibData.getStr(bfURL));
    assertEquals(List.of("0039-3681"), risBibData.getMultiStr(bfISSNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Adams, Fred", authorList.get(0).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """

      TY  - JOUR
      TI  - The Animal Sensorimotor Organization: A Challenge for the Environmental Complexity Thesis
      AU  - Keizer, Fred
      AU  - Arnellos, Argyris
      T2  - Philosophical Psychology
      DA  - 2017///
      PY  - 2017
      DO  - 10.1007/s10539-017-9565-3
      VL  - 26
      IS  - 4
      SP  - 502
      EP  - 519
      SN  - 0951-5089 1465-394X
      UR  - http://link.springer.com/article/10.1007/s10539-017-9565-3/fulltext.html
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("The Animal Sensorimotor Organization: A Challenge for the Environmental Complexity Thesis", risBibData.getStr(bfTitle));
    assertEquals("Philosophical Psychology", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2017)), risBibData.getDate());
    assertEquals("10.1007/s10539-017-9565-3", risBibData.getStr(bfDOI));
    assertEquals("26", risBibData.getStr(bfVolume));
    assertEquals("4", risBibData.getStr(bfIssue));
    assertEquals("502-519", risBibData.getStr(bfPages));
    assertEquals("http://link.springer.com/article/10.1007/s10539-017-9565-3/fulltext.html", risBibData.getStr(bfURL));
    assertEquals(List.of("0951-5089", "1465-394X"), risBibData.getMultiStr(bfISSNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(2, authorList.size());
    assertEquals("Keizer, Fred", authorList.get(0).getName().getLastFirst());
    assertEquals("Arnellos, Argyris"  , authorList.get(1).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """
      TY  - JOUR
      TI  - The Sphex Story: How the Cognitive Sciences Kept Repeating an Old and Questionable Anecdote
      AU  - Keizer, Fred
      AU  - Arnellos, Argyris
      T2  - Biology & Philosophy
      DA  - 2013///
      PY  - 2013
      DO  - 10.1080/09515089.2012.690177
      VL  - 32
      IS  - 3
      SP  - 421
      EP  - 441
      SN  - 0169-3867 1572-8404
      UR  - http://www.tandfonline.com/doi/pdf/10.1080/09515089.2012.690177
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("The Sphex Story: How the Cognitive Sciences Kept Repeating an Old and Questionable Anecdote", risBibData.getStr(bfTitle));
    assertEquals("Biology & Philosophy", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 0, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2013)), risBibData.getDate());
    assertEquals("10.1080/09515089.2012.690177", risBibData.getStr(bfDOI));
    assertEquals("32", risBibData.getStr(bfVolume));
    assertEquals("3", risBibData.getStr(bfIssue));
    assertEquals("421-441", risBibData.getStr(bfPages));
    assertEquals("http://www.tandfonline.com/doi/pdf/10.1080/09515089.2012.690177", risBibData.getStr(bfURL));
    assertEquals(List.of("0169-3867", "1572-8404"), risBibData.getMultiStr(bfISSNs));
    assertEquals("", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(2, authorList.size());
    assertEquals("Keizer, Fred", authorList.get(0).getName().getLastFirst());
    assertEquals("Arnellos, Argyris"  , authorList.get(1).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

    risExample = """
      TY  - JOUR
      TI  - Animal Agency
      AU  - Steward, Helen
      T2  - Inquiry
      AB  - Are animals agents? This question demands a prior answer to the question of what an agent is. The paper argues that we ought not to think of this as merely a matter of choosing from a range of alternative definitional stipulations. Evidence from developmental psychology is offered in support of the view that a basic concept of agency is a very early natural acquisition, which is established prior to the development of any full-blown propositional attitude concepts. Then it is argued that whatever one makes of the developmental evidence, it is in any case arguable on other grounds that the concept of agency as we have it in adulthood remains perfectly comprehensible independently of any reference to the more sophisticated propositional attitudes. Any reluctance we might feel to ascribe such things as beliefs to non-human animals, therefore, need not stand in the way of the claim that they are agents, nevertheless. The paper attempts to characterise the core of this basic agency concept, and discusses, albeit briefly, the question how we ought to decide which animals are to be thought of as falling under it. It concludes with some speculations about the nature of the intellectual currents which have made the shape of this important concept so hard for us to discern.
      DA  - 2009/05/21/
      PY  - 2009
      DO  - 10.1080/00201740902917119
      DP  - DOI.org (Crossref)
      VL  - 52
      IS  - 3
      SP  - 217
      EP  - 231
      J2  - Inquiry
      LA  - en
      SN  - 0020-174X, 1502-3923
      UR  - http://www.tandfonline.com/doi/abs/10.1080/00201740902917119
      Y2  - 2020/05/18/00:51:32
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Animal Agency", risBibData.getStr(bfTitle));
    assertEquals("Inquiry", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(21, 5, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2009)), risBibData.getDate());
    assertEquals("10.1080/00201740902917119", risBibData.getStr(bfDOI));
    assertEquals("en", risBibData.getStr(bfLanguage));
    assertEquals("52", risBibData.getStr(bfVolume));
    assertEquals("3", risBibData.getStr(bfIssue));
    assertEquals("217-231", risBibData.getStr(bfPages));
    assertEquals("http://www.tandfonline.com/doi/abs/10.1080/00201740902917119", risBibData.getStr(bfURL));
    assertEquals(List.of("0020-174X", "1502-3923"), risBibData.getMultiStr(bfISSNs));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Steward, Helen", authorList.get(0).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

    String expectedMisc = """
      Are animals agents? This question demands a prior answer to the question of what an agent is. The paper argues that we ought not to think of this as merely a matter of choosing from a range of alternative definitional stipulations. Evidence from developmental psychology is offered in support of the view that a basic concept of agency is a very early natural acquisition, which is established prior to the development of any full-blown propositional attitude concepts. Then it is argued that whatever one makes of the developmental evidence, it is in any case arguable on other grounds that the concept of agency as we have it in adulthood remains perfectly comprehensible independently of any reference to the more sophisticated propositional attitudes. Any reluctance we might feel to ascribe such things as beliefs to non-human animals, therefore, need not stand in the way of the claim that they are agents, nevertheless. The paper attempts to characterise the core of this basic agency concept, and discusses, albeit briefly, the question how we ought to decide which animals are to be thought of as falling under it. It concludes with some speculations about the nature of the intellectual currents which have made the shape of this important concept so hard for us to discern.
      Inquiry
      DOI.org (Crossref)""";

    assertEquals(expectedMisc, strListToStr(risBibData.getMultiStr(bfMisc), false, false));

//---------------------------------------------------------------------------

    risExample = """

      TY  - JOUR
      TI  - Why do humans reason? Arguments for an argumentative theory
      AU  - Mercier, Hugo
      AU  - Sperber, Dan
      T2  - Behavioral and Brain Sciences
      AB  - Reasoning is generally seen as a means to improve knowledge and make better decisions. However, much evidence shows that reasoning often leads to epistemic distortions and poor decisions. This suggests that the function of reasoning should be rethought. Our hypothesis is that the function of reasoning is argumentative. It is to devise and evaluate arguments intended to persuade. Reasoning so conceived is adaptive given the exceptional dependence of humans on communication and their vulnerability to misinformation. A wide range of evidence in the psychology of reasoning and decision making can be reinterpreted and better explained in the light of this hypothesis. Poor performance in standard reasoning tasks is explained by the lack of argumentative context. When the same problems are placed in a proper argumentative setting, people turn out to be skilled arguers. Skilled arguers, however, are not after the truth but after arguments supporting their views. This explains the notorious conﬁrmation bias. This bias is apparent not only when people are actually arguing, but also when they are reasoning proactively from the perspective of having to defend their opinions. Reasoning so motivated can distort evaluations and attitudes and allow erroneous beliefs to persist. Proactively used reasoning also favors decisions that are easy to justify but not necessarily better. In all these instances traditionally described as failures or ﬂaws, reasoning does exactly what can be expected of an argumentative device: Look for arguments that support a given conclusion, and, ceteris paribus, favor conclusions for which arguments can be found.
      DA  - 2011/04//
      PY  - 2011
      DO  - 10.1017/S0140525X10000968
      DP  - DOI.org (Crossref)
      VL  - 34
      IS  - 2
      SP  - 57
      EP  - 74
      J2  - Behav Brain Sci
      LA  - en
      SN  - 0140-525X, 1469-1825
      ST  - Why do humans reason?
      UR  - https://www.cambridge.org/core/product/identifier/S0140525X10000968/type/journal_article
      Y2  - 2020/05/18/00:51:42
      ER  -
      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etJournalArticle, risBibData.getEntryType());
    assertEquals("Why do humans reason? Arguments for an argumentative theory", risBibData.getStr(bfTitle));
    assertEquals("Behavioral and Brain Sciences", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(0, 4, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2011)), risBibData.getDate());
    assertEquals("10.1017/S0140525X10000968", risBibData.getStr(bfDOI));
    assertEquals("en", risBibData.getStr(bfLanguage));
    assertEquals("34", risBibData.getStr(bfVolume));
    assertEquals("2", risBibData.getStr(bfIssue));
    assertEquals("57-74", risBibData.getStr(bfPages));
    assertEquals("https://www.cambridge.org/core/product/identifier/S0140525X10000968/type/journal_article", risBibData.getStr(bfURL));
    assertEquals(List.of("0140-525X", "1469-1825"), risBibData.getMultiStr(bfISSNs));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(2, authorList.size());
    assertEquals("Mercier, Hugo", authorList.get(0).getName().getLastFirst());
    assertEquals("Sperber, Dan"  , authorList.get(1).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

    expectedMisc = """
      Reasoning is generally seen as a means to improve knowledge and make better decisions. However, much evidence shows that reasoning often leads to epistemic distortions and poor decisions. This suggests that the function of reasoning should be rethought. Our hypothesis is that the function of reasoning is argumentative. It is to devise and evaluate arguments intended to persuade. Reasoning so conceived is adaptive given the exceptional dependence of humans on communication and their vulnerability to misinformation. A wide range of evidence in the psychology of reasoning and decision making can be reinterpreted and better explained in the light of this hypothesis. Poor performance in standard reasoning tasks is explained by the lack of argumentative context. When the same problems are placed in a proper argumentative setting, people turn out to be skilled arguers. Skilled arguers, however, are not after the truth but after arguments supporting their views. This explains the notorious conﬁrmation bias. This bias is apparent not only when people are actually arguing, but also when they are reasoning proactively from the perspective of having to defend their opinions. Reasoning so motivated can distort evaluations and attitudes and allow erroneous beliefs to persist. Proactively used reasoning also favors decisions that are easy to justify but not necessarily better. In all these instances traditionally described as failures or ﬂaws, reasoning does exactly what can be expected of an argumentative device: Look for arguments that support a given conclusion, and, ceteris paribus, favor conclusions for which arguments can be found.
      Why do humans reason?
      Behav Brain Sci
      DOI.org (Crossref)""";

    assertEquals(expectedMisc, strListToStr(risBibData.getMultiStr(bfMisc), false, false));

//---------------------------------------------------------------------------

    risExample = """
      TY  - BLOG
      TI  - 3 global health issues to watch in 2021
      AU  - Branswell, Helen
      T2  - STAT
      AB  - The most annus horribilis of years in decades is nearly over. Looking to 2021, there are reasons for hope, @HelenBranswell writes.
      DA  - 2020/12/29/T09:30:59+00:00
      PY  - 2020
      LA  - en-US
      UR  - https://www.statnews.com/2020/12/29/global-health-2021-covid-who-polio/
      Y2  - 2024/09/22/01:30:17
      ER  -

      """;

    risBibData = RISBibData.create(convertMultiLineStrToStrList(risExample, true));

    assertEquals(etBlogPost, risBibData.getEntryType());
    assertEquals("3 global health issues to watch in 2021", risBibData.getStr(bfTitle));
    assertEquals("STAT", risBibData.getStr(bfContainerTitle));
    assertEquals(new BibliographicDate(29, 12, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(2020)), risBibData.getDate());
    assertEquals("en-US", risBibData.getStr(bfLanguage));
    assertEquals("The most annus horribilis of years in decades is nearly over. Looking to 2021, there are reasons for hope, @HelenBranswell writes.", risBibData.getStr(bfMisc));

    risBibData.getAuthors().getLists(authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Branswell, Helen", authorList.get(0).getName().getLastFirst());

    assertEquals(0, editorList.size());
    assertEquals(0, translatorList.size());

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
