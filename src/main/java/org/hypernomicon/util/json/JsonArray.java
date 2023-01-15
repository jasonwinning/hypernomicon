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

package org.hypernomicon.util.json;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.json.JsonObj.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class JsonArray implements Cloneable
{
  final JSONArray jArr;

  public JsonArray(JSONArray jArr) { this.jArr = jArr; }
  public JsonArray()               { jArr = new JSONArray(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                     { jArr.clear(); }
  public JsonObj getObj(int ndx)          { return new JsonObj((JSONObject) jArr.get(ndx)); }
  public JsonArray getArray(int ndx)      { return new JsonArray((JSONArray) jArr.get(ndx)); }
  public int size()                       { return jArr.size(); }
  public boolean isEmpty()                { return jArr.isEmpty(); }
  public JsonNodeType getType(int ndx)    { return determineType(jArr.get(ndx)); }
  public String getLongAsStrSafe(int ndx) { return nullSwitch(jArr.get(ndx), "", obj -> String.valueOf(((Long)obj).longValue())); }
  public JsonObjIterator getObjs()        { return new JsonObjIterator(); }
  public Stream<JsonObj> objStream()      { return ((Stream<?>)jArr.stream()).map(obj -> new JsonObj((JSONObject) obj)); }
  public Stream<String> strStream()       { return ((Stream<?>)jArr.stream()).map(obj -> obj instanceof String ? (String)obj : ""); }


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
        lastNdx = nextNdx++;
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

  @Override public final JsonArray clone()
  {
    try { return new JsonArray((JSONArray)jsonParser.parse(jArr.toJSONString())); } catch (ParseException e) { return null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(int ndx)
  {
    Object obj = jArr.get(ndx);
    return obj instanceof String ? (String)obj : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ArrayList<String> toStrArrayList(JsonArray jArr)
  {
    return jArr == null ? new ArrayList<>() : jArr.strStream().collect(Collectors.toCollection(ArrayList::new));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
