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

package org.hypernomicon.view.controls;

import javafx.geometry.Side;

/**
 * Custom version of the ControlsFX HiddenSidesPane class that exposes
 * the hide and show methods
 * 
 * @author  Jason Winning
 * @since   1.0
 */
public class HiddenSidesPane extends org.controlsfx.control.HiddenSidesPane
{
  public HiddenSidesPane()                         { setSkin(new HiddenSidesPaneSkin(this)); }
  
  public void hide()                               { HiddenSidesPaneSkin.class.cast(getSkin()).hide(); }  
  public void show(Side side, boolean noInterrupt) { HiddenSidesPaneSkin.class.cast(getSkin()).show(side, noInterrupt); }
}