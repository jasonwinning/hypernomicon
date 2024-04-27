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

package org.hypernomicon.view.wrappers;

import javafx.scene.Node;
import javafx.scene.control.ComboBox;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

@FunctionalInterface public interface CommitableWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void commit();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void commitWrapper(Node node)
  {
    if (node == null) return;

    if (node instanceof CommitableWrapper commitableWrapper)
    {
      commitableWrapper.commit();
      return;
    }

    if (node instanceof ComboBox)
    {
      HyperCB hcb = (HyperCB) getNodeUserObj(node, NodeUserDataType.HypercCB);

      if ((hcb != null) && hcb.autoCommitBeforeRecordSave)
      {
        hcb.commit();
        return;
      }
    }

    nullSwitch(node.getParent(), CommitableWrapper::commitWrapper);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
