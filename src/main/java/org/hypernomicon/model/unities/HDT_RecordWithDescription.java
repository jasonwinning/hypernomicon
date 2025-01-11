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

package org.hypernomicon.model.unities;

import org.hypernomicon.model.records.HDT_Record;

/**
 * Instantiating this class implies that a record has at least one description field. Calling {@code getDesc()}
 * retrieves the default description field.<br>
 * <br>
 * This class differs from {@link org.hypernomicon.model.unities.HDT_RecordWithMainText HDT_RecordWithMainText}
 * because unlike this class, {@code HDT_RecordWithMainText} records will always have a description field that is considered
 * to be the "main" one, whereas implementing {@link HDT_RecordWithDescription HDT_RecordWithDescription}
 * only implies that there is at least one description field but none that is
 * considered to be the "main" one. For example, {@link org.hypernomicon.model.records.HDT_Term HDT_Term}
 * instantiates {@code HDT_RecordWithDescription} but not {@code HDT_RecordWithMainText} because there can be multiple
 * definitions (multiple {@link org.hypernomicon.model.records.HDT_Concept HDT_Concept} records). Hence,
 * {@code HDT_Term} records are united one definition at a time.<br>
 * <br>
 *
 * @author  Jason Winning
 * @since   1.0
 */
public interface HDT_RecordWithDescription extends HDT_Record
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText getDesc();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
