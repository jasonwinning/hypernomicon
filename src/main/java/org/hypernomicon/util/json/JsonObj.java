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

import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.Reader;
import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;
import org.json.simple.parser.JSONParser;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class JsonObj implements Cloneable
{
  public enum JsonNodeType { OBJECT, STRING, ARRAY, BOOLEAN, INTEGER, NONE }

  public static final JSONParser jsonParser = new JSONParser();

  final JSONObject jObj;

  public JsonObj(JSONObject jObj) { this.jObj = jObj; }
  public JsonObj()                { jObj = new JSONObject(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                     { jObj.clear(); }
  public boolean containsKey(String key)  { return jObj.containsKey(key); }
  public void remove(String key)          { jObj.remove(key); }
  public JsonNodeType getType(String key) { return determineType(jObj.get(key)); }

  public JsonObj getObj(String key)         { return nullSwitch((JSONObject)jObj.get(key), null, JsonObj::new); }
  public String getStr(String key)          { return (String) jObj.get(key); }
  public String getAsStr(String key)        { return nullSwitch(jObj.get(key), "", Object::toString); }
  public JsonArray getArray(String key)     { return nullSwitch((JSONArray)jObj.get(key), null, JsonArray::new); }

  @SuppressWarnings("unchecked") public void putNull(String key)               { jObj.put(key, null); }
  @SuppressWarnings("unchecked") public void put(String key, JsonObj childObj) { jObj.put(key, childObj.jObj); }
  @SuppressWarnings("unchecked") public void put(String key, String value)     { jObj.put(key, value); }
  @SuppressWarnings("unchecked") public void put(String key, JsonArray value)  { jObj.put(key, value.jArr); }
  @SuppressWarnings("unchecked") public void put(String key, Long value)       { jObj.put(key, value); }
  @SuppressWarnings("unchecked") public Set<String> keySet()                   { return jObj.keySet(); }

  @Override public String toString() { return jObj.toJSONString(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public long getLong(String key, long def)
  {
    Object obj = jObj.get(key);

    return obj instanceof String ?
      parseLong(getStr(key), def)
    :
      nullSwitch((Long)jObj.get(key), def, Long::longValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getBoolean(String key, boolean def)
  {
    Object obj = jObj.get(key);

    return obj instanceof String ?
      parseBoolean(getStr(key))
    :
      (obj instanceof Boolean ? (Boolean) obj : def);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStrSafe(String key)
  {
    Object obj = jObj.get(key);
    return obj instanceof String ? (String)obj : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final JsonObj clone()
  {
    JsonObj otherObj = null;

    try { otherObj = parseJsonObj(jObj.toJSONString()); } catch (ParseException e) { noOp(); }

    return otherObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static JsonNodeType determineType(Object object)
  {
    if (object instanceof JSONObject) return JsonNodeType.OBJECT;
    if (object instanceof JSONArray ) return JsonNodeType.ARRAY;
    if (object instanceof String    ) return JsonNodeType.STRING;
    if (object instanceof Boolean   ) return JsonNodeType.BOOLEAN;
    if (object instanceof Long      ) return JsonNodeType.INTEGER;

    return JsonNodeType.NONE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static JsonObj parseJsonObj(Reader in) throws IOException, org.json.simple.parser.ParseException
  { return new JsonObj((JSONObject) jsonParser.parse(in)); }

  private static JsonObj parseJsonObj(String str) throws org.json.simple.parser.ParseException
  { return new JsonObj((JSONObject) jsonParser.parse(str)); }

  public static JsonArray parseJson(String str) throws org.json.simple.parser.ParseException
  { return wrapJSONObject(jsonParser.parse(str)); }

  public static JsonArray parseJson(Reader in) throws IOException, org.json.simple.parser.ParseException
  { return wrapJSONObject(jsonParser.parse(in)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private static JsonArray wrapJSONObject(Object obj)
  {
    if (obj instanceof JSONObject)
    {
      JSONArray jArr = new JSONArray();
      jArr.add(obj);
      return new JsonArray(jArr);
    }

    return obj instanceof JSONArray ? new JsonArray((JSONArray) obj) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
