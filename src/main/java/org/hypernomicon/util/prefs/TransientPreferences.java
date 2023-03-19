/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.util.prefs;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.prefs.AbstractPreferences;

class TransientPreferences extends AbstractPreferences
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Map<String, TransientPreferences> children = new LinkedHashMap<>();
  private final Map<String, String>               values   = new LinkedHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  TransientPreferences()                                                 { this(null, ""); }

  private TransientPreferences(TransientPreferences parent, String name) { super(parent, name); }


  @Override public boolean isUserNode()                     { return true; }
  @Override protected boolean isRemoved()                   { return super.isRemoved(); } // Increase visibility
  @Override protected void putSpi(String key, String value) { values.put(key, value); }
  @Override protected String getSpi(String key)             { return values.get(key); } // returning null is okay because that can trigger a default value being used instead
  @Override protected void removeSpi(String key)            { values.remove(key); }

  @Override protected void removeNodeSpi       () { values.clear(); }
  @Override protected String[] keysSpi         () { return values  .keySet().toArray(new String[0]); }
  @Override protected String[] childrenNamesSpi() { return children.keySet().toArray(new String[0]); }

  @Override protected void syncSpi             () { /* not used */ }
  @Override protected void flushSpi            () { /* not used */ }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected AbstractPreferences childSpi(String name)
  {
    TransientPreferences child = children.get(name);

    if ((child == null) || child.isRemoved())
    {
      child = new TransientPreferences(this, name);
      children.put(name, child);
    }

    return child;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
