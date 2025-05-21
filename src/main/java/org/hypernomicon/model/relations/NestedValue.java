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

package org.hypernomicon.model.relations;

import org.hypernomicon.model.HDI_Schema.HyperDataCategory;
import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HDI_OnlineBase;
import org.hypernomicon.model.items.HDI_OnlineBoolean;
import org.hypernomicon.model.items.HDI_OnlineNestedPointer;
import org.hypernomicon.model.items.HDI_OnlineString;
import org.hypernomicon.model.items.HDI_OnlineTernary;
import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class NestedValue
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String str = "";
  public boolean bool = false;
  public Ternary ternary = Ternary.Unset;
  public HDT_Record target = null;
  final HyperDataCategory hdc;

//---------------------------------------------------------------------------

  NestedValue(HDI_OnlineBase<? extends HDI_OfflineBase> item)
  {
    this(item.category());

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

  public NestedValue(HyperDataCategory hdc)
  {
    this.hdc = hdc;
  }

//---------------------------------------------------------------------------

  public static boolean isEmpty(String str)        { return strNullOrEmpty(str); }
  public static boolean isEmpty(boolean bool)      { return !bool; }
  public static boolean isEmpty(Ternary ternary)   { return ternary.isUnset(); }
  public static boolean isEmpty(int id)            { return id < 1; }
  public static boolean isEmpty(HDT_Record target) { return HDT_Record.isEmpty(target, false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;

    result = prime * result + (hdc == null ? 0 : hdc.hashCode());

    int valHashCode = 0;

    if ((hdc != null) && (isEmpty() == false))
    {
      valHashCode = switch (hdc)
      {
        case hdcString        -> str.hashCode();
        case hdcBoolean       -> Boolean.hashCode(bool);
        case hdcTernary       -> ternary.hashCode();
        case hdcNestedPointer -> target.hashCode();
        default               -> 0;
      };
    }

    return prime * result + valHashCode;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return isEmpty();
    if (getClass() != obj.getClass()) return false;

    NestedValue other = (NestedValue) obj;

    if (isEmpty() && other.isEmpty()) return true;

    return (hdc == other.hdc) && switch (hdc)
    {
      case hdcString        -> str.equals(other.str);
      case hdcBoolean       -> bool == other.bool;
      case hdcTernary       -> ternary == other.ternary;
      case hdcNestedPointer -> target == other.target;
      default               -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty() { return switch (hdc)
  {
    case hdcString        -> isEmpty(str);
    case hdcBoolean       -> isEmpty(bool);
    case hdcTernary       -> isEmpty(ternary);
    case hdcNestedPointer -> HDT_Record.isEmpty(target, false);
    default               -> true;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
