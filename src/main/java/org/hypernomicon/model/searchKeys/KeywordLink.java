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

package org.hypernomicon.model.searchKeys;

import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class KeywordLink
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final int offset, length;
  private final KeywordBinding binding;
  private final HDT_Record record;
  private Set<KeywordBinding> bindings;

//---------------------------------------------------------------------------

  KeywordLink(int offset, int length, Collection<KeywordBinding> bindings)
  {
    this.offset = offset;
    this.length = length;
    this.bindings = Set.copyOf(bindings);

    binding = null;
    record = null;
  }

  public KeywordLink(int offset, int length, KeywordBinding binding)
  {
    this.offset = offset;
    this.length = length;
    this.binding = binding;

    bindings = null;
    record = binding.getRecord();
  }

//---------------------------------------------------------------------------

  public int getOffset()             { return offset; }
  public int getLength()             { return length; }
  public boolean isSingle()          { return binding != null; }
  public KeywordBinding getBinding() { return binding; }
  public HDT_Record getRecord()      { return record; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Collection<KeywordBinding> getAllBindings()
  {
    if (bindings == null)
      bindings = Set.of(binding);

    return bindings;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<HDT_Record> recordStream()
  {
    if (bindings == null)
      return Stream.of(record);

    return bindings.stream().map(KeywordBinding::getRecord).distinct();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
