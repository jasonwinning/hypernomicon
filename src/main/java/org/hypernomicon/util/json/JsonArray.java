/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.util.json;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.hypernomicon.util.json.JsonObj.JsonNodeType;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class JsonArray
{
  JSONArray jArr;

  public JsonArray(JSONArray jArr) { this.jArr = jArr; }
  public JsonArray()               { jArr = new JSONArray(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                     { jArr.clear(); }
  public JsonObj getObj(int ndx)          { return new JsonObj((JSONObject) jArr.get(ndx)); }
  public JsonArray getArray(int ndx)      { return new JsonArray((JSONArray) jArr.get(ndx)); }
  public boolean contains(String collKey) { return jArr.contains(collKey); }
  public int size()                       { return jArr.size(); }
  public JsonNodeType getType(int ndx)    { return JsonObj.determineType(jArr.get(ndx)); }
  public String getLongAsStrSafe(int ndx) { return nullSwitch(jArr.get(ndx), "", obj -> String.valueOf(Long.class.cast(obj).longValue())); }
  public JsonObjIterator getObjs()        { return new JsonObjIterator(); }
  public JsonStrIterator getStrs()        { return new JsonStrIterator(); }

  @SuppressWarnings("unchecked") public void set(int ndx, JsonObj element)   { jArr.set(ndx, element); }
  @SuppressWarnings("unchecked") public void set(int ndx, JsonArray element) { jArr.set(ndx, element); }
  @SuppressWarnings("unchecked") public void set(int ndx, String element)    { jArr.set(ndx, element); }

  @SuppressWarnings("unchecked") public void add(int ndx, JsonObj element)   { jArr.add(ndx, element.jObj); }
  @SuppressWarnings("unchecked") public void add(int ndx, JsonArray element) { jArr.add(ndx, element.jArr); }
  @SuppressWarnings("unchecked") public void add(int ndx, String element)    { jArr.add(ndx, element); }

  @SuppressWarnings("unchecked") public void add(JsonObj element)   { jArr.add(element.jObj); }
  @SuppressWarnings("unchecked") public void add(JsonArray element) { jArr.add(element.jArr); }
  @SuppressWarnings("unchecked") public void add(String element)    { jArr.add(element); }

  @Override public String toString() { return jArr.toJSONString(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class JsonObjIterator implements Iterator<JsonObj>, Iterable<JsonObj>
  {
    private int lastNdx = -1, nextNdx = 0;

    //---------------------------------------------------------------------------

    @Override public boolean hasNext()            { return nextNdx < jArr.size(); }
    @Override public Iterator<JsonObj> iterator() { return this; }

  //---------------------------------------------------------------------------

    @Override public JsonObj next()
    {
      if (hasNext())
      {
        lastNdx = nextNdx;
        nextNdx++;
        return getObj(lastNdx);
      }

      throw new NoSuchElementException();
    }

  //---------------------------------------------------------------------------

    @Override public void remove()
    {
      if (lastNdx == -1)
        throw new IllegalStateException();

      jArr.remove(lastNdx);
      nextNdx--;
      lastNdx = -1;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class JsonStrIterator implements Iterator<String>, Iterable<String>
  {
    private int lastNdx = -1, nextNdx = 0;

  //---------------------------------------------------------------------------

    @Override public boolean hasNext()           { return nextNdx < jArr.size(); }
    @Override public Iterator<String> iterator() { return this; }

  //---------------------------------------------------------------------------

    @Override public String next()
    {
      if (hasNext())
      {
        lastNdx = nextNdx;
        nextNdx++;
        return getStr(lastNdx);
      }

      throw new NoSuchElementException();
    }

  //---------------------------------------------------------------------------

    @Override public void remove()
    {
      if (lastNdx == -1)
        throw new IllegalStateException();

      jArr.remove(lastNdx);
      nextNdx--;
      lastNdx = -1;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(int ndx)
  {
    Object obj = jArr.get(ndx);
    return obj instanceof String ? String.class.cast(obj) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
