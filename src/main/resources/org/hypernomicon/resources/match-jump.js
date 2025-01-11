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

function jumpTo()
{
  clearCurrent();
  
  if (results.length)
  {
    var current = results[currentNdx];
    if (typeof current === "undefined") return;
    current.classList.add("hypernomiconHiliteCurrent");
    current.scrollIntoView();
    lastNdx = currentNdx;
  }
}

function clearAll()
{
  clearCurrent();
  
  Array.from(document.getElementsByClassName('hypernomiconHilite')).forEach(element =>
  {
    if (element.tagName === 'A')
      element.classList.remove("hypernomiconHilite");
  });
}

function clearCurrent()
{
  if (lastNdx > -1)
  {
    var current = results[lastNdx];
    if (typeof current === "undefined") return;
    current.classList.remove("hypernomiconHiliteCurrent");
    lastNdx = 0;
  }
}
    
function nextResult()
{
  if (results.length)
  {
    currentNdx = currentNdx + 1;
    if (currentNdx >= results.length)
    {
      currentNdx = 0;
    }
    jumpTo();
  }
}

function previousResult()
{
  if (results.length)
  {
    currentNdx = currentNdx - 1;
    if (currentNdx < 0)
    {
      currentNdx = results.length - 1;
    }
    jumpTo();
  }
}
