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

package org.hypernomicon.model.searchKeys;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class Keyword
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final String normalizedText; // Will always be the same between bindings

  private final Map<HDT_Record, KeywordBinding> bindings;

//---------------------------------------------------------------------------

  public Collection<KeywordBinding> getAllBindings() { return Collections.unmodifiableCollection(bindings.values()); }
  public Collection<HDT_Record>     getAllRecords () { return Collections.unmodifiableCollection(bindings.keySet()); }

//---------------------------------------------------------------------------

  public Keyword(KeywordBinding binding)
  {
    if (binding == null)
      throw new IllegalArgumentException("Keyword must be constructed with a non-null binding");

    this.normalizedText = binding.getNormalizedText();
    this.bindings = new ConcurrentHashMap<>();
    this.bindings.put(binding.getRecord(), binding);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addBinding(KeywordBinding binding)
  {
    if (binding == null)
      throw new IllegalArgumentException("Cannot add a null binding");

    if (binding.getNormalizedText().equals(normalizedText) == false)
      throw new IllegalArgumentException("Binding text does not match Keyword text");

    bindings.put(binding.getRecord(), binding);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes a binding from a Keyword
   * @param record The record for the binding to remove
   * @return True if this Keyword no longer contains any bindings; false otherwise
   */
  boolean removeBinding(HDT_Record record)
  {
    if (record == null)
      throw new IllegalArgumentException("Cannot remove binding for null record");

    if (bindings.remove(record) == null)
      throw new IllegalArgumentException("Record does not match any existing bindings");

    return bindings.isEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
