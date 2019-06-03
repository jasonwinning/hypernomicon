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

import static org.hypernomicon.util.Util.*;

import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class JsonObj implements Cloneable
{
  public static enum JsonNodeType { OBJECT, STRING, ARRAY, BOOLEAN, NONE }

  final JSONObject jObj;

  public JsonObj(JSONObject jObj) { this.jObj = jObj; }
  public JsonObj()                { jObj = new JSONObject(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Object getRawValue(String key)   { return jObj.get(key); }
  public void clear()                     { jObj.clear(); }
  public boolean containsKey(String key)  { return jObj.containsKey(key); }
  public void remove(String key)          { jObj.remove(key); }
  public JsonNodeType getType(String key) { return JsonObj.determineType(jObj.get(key)); }

  public JsonObj getObj(String key)         { return nullSwitch((JSONObject)jObj.get(key), null, JsonObj::new); }
  public String getStr(String key)          { return (String) jObj.get(key); }
  public JsonArray getArray(String key)     { return nullSwitch((JSONArray)jObj.get(key), null, JsonArray::new); }
  public long getLong(String key, long def) { return nullSwitch((Long)jObj.get(key), def, Long::longValue); }

  @SuppressWarnings("unchecked") public void put(String key, JsonObj childObj) { jObj.put(key, childObj.jObj); }
  @SuppressWarnings("unchecked") public void put(String key, String value)     { jObj.put(key, value); }
  @SuppressWarnings("unchecked") public void put(String key, JsonArray value)  { jObj.put(key, value.jArr); }
  @SuppressWarnings("unchecked") public Set<String> keySet()                   { return jObj.keySet(); }

  @Override public String toString() { return jObj.toJSONString(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getBoolean(String key, boolean def)
  {
    Object obj = jObj.get(key);

    if (obj instanceof String)
      return parseBoolean(getStr(key));

    return obj instanceof Boolean ? Boolean.class.cast(obj).booleanValue() : def;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStrSafe(String key)
  {
    Object obj = jObj.get(key);
    return obj instanceof String ? String.class.cast(obj) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final JsonObj clone()
  {
    JsonObj otherObj = null;

    try { otherObj = parseJsonObj(jObj.toJSONString()); } catch (ParseException e) { noOp(); }

    return nullSwitch(otherObj, new JsonObj(null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static JsonNodeType determineType(Object object)
  {
    if (object instanceof JSONObject) return JsonNodeType.OBJECT;
    if (object instanceof JSONArray ) return JsonNodeType.ARRAY;
    if (object instanceof String    ) return JsonNodeType.STRING;
    if (object instanceof Boolean   ) return JsonNodeType.BOOLEAN;

    return JsonNodeType.NONE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
