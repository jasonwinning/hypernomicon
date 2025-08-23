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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.*;

import org.hypernomicon.model.items.BibliographicDate;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

public class DateControlsWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class DateProperty extends SimpleObjectProperty<BibliographicDate>
  {
    private DateProperty(BibliographicDate initialValue, String name)
    {
      super(initialValue == null ? BibliographicDate.EMPTY_DATE : initialValue, name);
      setValue(initialValue);
    }

    @Override public void set(BibliographicDate newValue)
    {
      super.set(newValue == null ? BibliographicDate.EMPTY_DATE : newValue);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final TextField tfYear, tfDay;
  private final ComboBox<Month> cbMonth;
  private final Property<BibliographicDate> value;

  private boolean programmaticChange = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public DateControlsWrapper(TextField tfYear, ComboBox<Month> cbMonth, TextField tfDay, BibliographicDate date)
  {
    this(tfYear, cbMonth, tfDay);

    setDate(date);
  }

//---------------------------------------------------------------------------

  public DateControlsWrapper(TextField tfYear, ComboBox<Month> cbMonth, TextField tfDay)
  {
    this.tfYear = tfYear;
    this.tfDay = tfDay;
    this.cbMonth = cbMonth;

    value = new DateProperty(BibliographicDate.EMPTY_DATE, "value");

    setDateValueToUI(getDate());

    setToolTip(tfYear , "Year" );
    setToolTip(cbMonth, "Month");
    setToolTip(tfDay  , "Day"  );

    SequencedMap<Month, String> strMap = new LinkedHashMap<>();

    for (int monthInt = 1; monthInt <= 12; monthInt++)
    {
      Month month = Month.of(monthInt);
      strMap.put(month, month.getDisplayName(TextStyle.SHORT, Locale.getDefault()));
    }

    strMap.put(null, "");

    SimpleSelector.init(cbMonth, strMap);

    tfDay.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.isAdded())
      {
        if (change.getText().matches(".*[^0-9].*"))
        {
          change.setText("");
          return change;
        }

        int intVal = parseInt(change.getControlNewText(), 0);

        if ((intVal < 1) || (intVal > 31))
          change.setText("");
      }

      return change;
    }));

    value.addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;
      programmaticChange = true;

      setDateValueToUI(nv);

      programmaticChange = false;
    });

    tfYear .textProperty ().addListener((obs, ov, nv) -> updateValue(tfDay.getText(), cbMonth.getSelectionModel().getSelectedItem(), nv              ));
    cbMonth.valueProperty().addListener((obs, ov, nv) -> updateValue(tfDay.getText(), nv                                           , tfYear.getText()));
    tfDay  .textProperty ().addListener((obs, ov, nv) -> updateValue(nv             , cbMonth.getSelectionModel().getSelectedItem(), tfYear.getText()));
  }

//---------------------------------------------------------------------------

  public Property<BibliographicDate> valueProperty() { return value; }
  public BibliographicDate getDate()                 { return value.getValue(); }
  public void setDate(BibliographicDate date)        { value.setValue(date); }
  public void clear()                                { value.setValue(BibliographicDate.EMPTY_DATE); }
  public void setDisable(boolean value)              { disableAllIff(value, tfYear, cbMonth, tfDay); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDateValueToUI(BibliographicDate nv)
  {
    tfYear.setText(nv.getYearStr());
    cbMonth.getSelectionModel().select(nv.hasMonth() ? Month.of(nv.month) : null);
    tfDay.setText(nv.hasDay() ? String.valueOf(nv.day) : "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateValue(String dayStr, Month month, String yearStr)
  {
    if (programmaticChange) return;
    programmaticChange = true;

    value.setValue(new BibliographicDate(parseInt(dayStr, 0), nullSwitch(month, 0, Month::getValue), yearStr, false));

    programmaticChange = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
