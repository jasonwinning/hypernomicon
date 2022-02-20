/*
 * Copyright 2015-2022 Jason Winning
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

import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.scene.control.ComboBoxBase;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;

public class UIUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MutableBoolean repositionPopupListWorkaround(MenuButton ctrl)
  {
    MutableBoolean adjusting = new MutableBoolean(false);

    ctrl.addEventFilter(MenuButton.ON_SHOWN, event ->  //////////////
    {                                                    //
      if (adjusting.isTrue()) return;                    //
                                                         // This is a workaround for the
      adjusting.setTrue();                               // fact that sometimes, when you show the
                                                         // popup list for a control, the popup list
      ctrl.hide();                                       // appears in the wrong place
      ctrl.show();                                       //
                                                         //
      adjusting.setFalse();                              //
    });                                                  //////////////

    return adjusting;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MutableBoolean repositionPopupListWorkaround(ComboBoxBase<?> ctrl)
  {
    MutableBoolean adjusting = new MutableBoolean(false);

    ctrl.addEventFilter(ComboBoxBase.ON_SHOWN, event ->  //////////////
    {                                                    //
      if (adjusting.isTrue()) return;                    //
                                                         // This is a workaround for the
      adjusting.setTrue();                               // fact that sometimes, when you show the
                                                         // popup list for a control, the popup list
      ctrl.hide();                                       // appears in the wrong place
      ctrl.show();                                       //
                                                         //
      adjusting.setFalse();                              //
    });                                                  //////////////

    return adjusting;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MenuItem copyOf(MenuItem orig)
  {
    MenuItem copy = new MenuItem(orig.getText());

    copy.setOnAction(orig.getOnAction());
    copy.setOnMenuValidation(orig.getOnMenuValidation());
    copy.setStyle(orig.getStyle());
    copy.setAccelerator(orig.getAccelerator());
    copy.setMnemonicParsing(orig.isMnemonicParsing());

    return copy;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
