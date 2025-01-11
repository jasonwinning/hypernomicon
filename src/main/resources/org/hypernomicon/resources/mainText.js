/*
 * Copyright 2023-2025 Jason Winning
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

var jsToJava = {};

var JS_EVENT_OPEN_RECORD         = 1;
var JS_EVENT_OPEN_PREVIEW        = 2;
var JS_EVENT_OPEN_URL            = 3;
var JS_EVENT_LAUNCH_FILE         = 4;
var JS_EVENT_OPEN_FILE           = 5;
var JS_EVENT_SET_SORT_KEY_METHOD = 6;
var JS_EVENT_DETAILED_KEY_WORKS  = 7;

var ALPHA_SORTED_OUTER_CLASS   = "sortedKeyWorksAZ";
var NUMERIC_SORTED_OUTER_CLASS = "sortedKeyWorks19";

function openFile(recordType, recordID)
{
  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(JS_EVENT_OPEN_FILE);
}

function openRecord(recordType, recordID)
{
  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(JS_EVENT_OPEN_RECORD);
}

function openPreview(recordType, recordID)
{
  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(JS_EVENT_OPEN_PREVIEW);
}

function openURL(url)
{
  jsToJava.url = url; callToJava(JS_EVENT_OPEN_URL);
}

function callToJava(eventType)
{
  jsToJava.eventType = eventType;
  jsToJava.eventID = (new Date()).getTime();
  jsToJava.scrollTop = document.body.scrollTop;
  document.title = "" + jsToJava.eventID;
}

function switchToAZ()
{
  var i,elements = document.getElementsByTagName('details');
  
  for (i=0; i<elements.length; i++)
  {
    if (elements[i].id.slice(0,3) === "num")
    {
      document.getElementById("alp" + elements[i].id.slice(3)).open = elements[i].open;
    }
  }
  
  elements = document.getElementsByClassName(NUMERIC_SORTED_OUTER_CLASS);
  
  for (i = 0; i < elements.length; i++)
  { 
    elements[i].style.display = 'none'; 
  }
  
  elements = document.getElementsByClassName(ALPHA_SORTED_OUTER_CLASS);
  
  for (i = 0; i < elements.length; i++)
  { 
    elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block');
  }
  
  jsToJava.sortByName = true; callToJava(JS_EVENT_SET_SORT_KEY_METHOD);
}

function switchTo19()
{
  var i,elements = document.getElementsByTagName('details');
  
  for (i = 0; i < elements.length; i++)
  {
    if (elements[i].id.slice(0,3) === "alp")
    {
      document.getElementById("num" + elements[i].id.slice(3)).open = elements[i].open;
    }
  }
  
  elements = document.getElementsByClassName(ALPHA_SORTED_OUTER_CLASS);
  
  for (i = 0; i < elements.length; i++) 
  { 
    elements[i].style.display = 'none'; 
  }
  
  elements = document.getElementsByClassName(NUMERIC_SORTED_OUTER_CLASS);
  
  for (i = 0; i < elements.length; i++) 
  { 
    elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block');
  }
  
  jsToJava.sortByName = false; callToJava(JS_EVENT_SET_SORT_KEY_METHOD);
}
