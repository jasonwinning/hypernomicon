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

package org.hypernomicon.view.controls;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.util.DPIScalableContainer;

import javafx.geometry.Side;
import javafx.scene.Node;

//---------------------------------------------------------------------------

/**
 * Custom version of the ControlsFX HiddenSidesPane class that exposes
 * the hide and show methods
 *
 * @author  Jason Winning
 * @since   1.0
 */
public class HiddenSidesPane extends org.controlsfx.control.HiddenSidesPane implements DPIScalableContainer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HiddenSidesPane()                         { setSkin(new HiddenSidesPaneSkin(this)); }

  @Override public void hide()                     { ((HiddenSidesPaneSkin) getSkin()).hide(); }
  public void show(Side side, boolean noInterrupt) { ((HiddenSidesPaneSkin) getSkin()).show(side, noInterrupt); }

  // The content and side nodes are held through this control's own properties;
  // declaring them keeps the DPI rescale independent of whether the skin (set
  // eagerly above) has built its children yet.

  @Override public List<Node> dpiScalableChildren() { return Arrays.asList(getContent(), getTop(), getRight(), getBottom(), getLeft()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
