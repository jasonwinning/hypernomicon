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

package org.hypernomicon.model.relations;

import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HDI_OnlineBase;
import org.hypernomicon.model.items.HDI_OnlineBoolean;
import org.hypernomicon.model.items.HDI_OnlineNestedPointer;
import org.hypernomicon.model.items.HDI_OnlineString;
import org.hypernomicon.model.items.HDI_OnlineTernary;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory;

import static org.hypernomicon.util.Util.*;

public class NestedValue
{
  public String str = "";
  public boolean bool = false;
  public Ternary ternary = Ternary.Unset;
  public HDT_Record target = null;
  public final HyperDataCategory hdc;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public NestedValue(HDI_OnlineBase<? extends HDI_OfflineBase> item)
  {
    this(item.getCategory());

    switch (hdc)
    {
      case hdcString        : str     = ((HDI_OnlineString       )item).get(); break;
      case hdcBoolean       : bool    = ((HDI_OnlineBoolean      )item).get(); break;
      case hdcTernary       : ternary = ((HDI_OnlineTernary      )item).get(); break;
      case hdcNestedPointer : target  = ((HDI_OnlineNestedPointer)item).get(); break;
      default               : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public NestedValue(HyperDataCategory hdc)
  {
    this.hdc = hdc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (bool ? 1231 : 1237);
    result = prime * result + (hdc == null ? 0 : hdc.hashCode());
    result = prime * result + (str == null ? 0 : str.hashCode());
    result = prime * result + (target == null ? 0 : target.hashCode());
    result = prime * result + (ternary == null ? 0 : ternary.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    NestedValue other = (NestedValue) obj;

    if (hdc != other.hdc) return false;

    switch (hdc)
    {
      case hdcString        : return str.equals(other.str);
      case hdcBoolean       : return bool == other.bool;
      case hdcTernary       : return ternary == other.ternary;
      case hdcNestedPointer : return target == other.target;
      default               : return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isEmpty(String str)        { return safeStr(str).isEmpty(); }
  public static boolean isEmpty(boolean bool)      { return !bool; }
  public static boolean isEmpty(Ternary ternary)   { return ternary == Ternary.Unset; }
  public static boolean isEmpty(int id)            { return id < 1; }
  public static boolean isEmpty(HDT_Record target) { return HDT_Record.isEmpty(target); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()
  {
    switch (hdc)
    {
      case hdcString        : return isEmpty(str);
      case hdcBoolean       : return isEmpty(bool);
      case hdcTernary       : return isEmpty(ternary);
      case hdcNestedPointer : return HDT_Record.isEmpty(target);
      default               : return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}