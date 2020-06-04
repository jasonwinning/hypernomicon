/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.model.records.HDT_Region;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class NewRegionDlgCtrlr extends HyperDlg
{
  @FXML private TextField tfName, tfAbbrev;
  @FXML private Label lblCountry;

  private HDT_Region region = null;
  private HDT_Country country = null;

  public HDT_Region getRegion() { return region; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewRegionDlgCtrlr build(HDT_Country country)
  {
    return ((NewRegionDlgCtrlr) create("NewRegionDlg", "New State/Region", true)).init(country);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewRegionDlgCtrlr init(HDT_Country country)
  {
    lblCountry.setText("Country: " + country.name());

    this.country = country;

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfName.getText().isBlank())
      return falseWithWarningMessage("You must enter a name.", tfName);

    if (tfAbbrev.getText().isBlank())
      return falseWithWarningMessage("You must enter an abbreviation.", tfAbbrev);

    for (HDT_Region region : db.regions)
    {
      if (ultraTrim(convertToEnglishChars(region.name())).equalsIgnoreCase(ultraTrim(convertToEnglishChars(tfName.getText()))))
        return falseWithWarningMessage("A state/region with the name " + region.name() + " already exists.", tfName);

      if (ultraTrim(convertToEnglishChars(region.getAbbreviation())).equalsIgnoreCase(ultraTrim(convertToEnglishChars(tfAbbrev.getText()))))
        return falseWithWarningMessage("A state/region with the abbreviation " + region.getAbbreviation() + " already exists.", tfName);
    }

    region = db.createNewBlankRecord(hdtRegion);
    region.setName(tfName.getText());
    region.setAbbreviation(tfAbbrev.getText());
    region.country.set(country);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
