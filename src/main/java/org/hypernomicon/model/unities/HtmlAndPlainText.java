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

package org.hypernomicon.model.unities;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

/**
 * The point of this class is to allow for lazy initialization of the
 * main HTML text field for many record types. If eager initialization
 * is used instead, initial database loading is significantly slower.
 *
 * @author  Jason Winning
 * @since   1.24
 */
class HtmlAndPlainText
{

//---------------------------------------------------------------------------

  private final String inputHtml;
  private String plainText = "", html = "";
  private boolean initialized = false;

  HtmlAndPlainText(String inputHtml)
  {
    this.inputHtml = inputHtml;
  }

  //---------------------------------------------------------------------------

  String getPlainText() { ensureInitialized(); return plainText; }
  String getHtml     () { ensureInitialized(); return html;      }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private synchronized void ensureInitialized()
  {
    if (initialized) return;

    if (inputHtml.isEmpty())
    {
      initialized = true;
      return;
    }

    plainText = extractTextFromHTML(inputHtml);

    if (ultraTrim(convertToSingleLine(plainText)).isEmpty() && (inputHtml.contains("&lt;" + EMBEDDED_FILE_TAG + ' ') == false))
    {
      html = "";
      plainText = "";
    }
    else
      html = safeStr(inputHtml);

    initialized = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
