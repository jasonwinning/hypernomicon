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

package org.hypernomicon.fts;

import static org.hypernomicon.util.Util.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.json.simple.parser.ParseException;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

//---------------------------------------------------------------------------

public final class DatabaseSketch
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final EnumMap<RecordType, int[]> typeToIDArray;

  private static final double SIMILARITY_THRESHOLD = 0.8;
  private static final int CURRENT_SKETCH_FORMAT_VERSION = 1;

//---------------------------------------------------------------------------

  private DatabaseSketch(EnumMap<RecordType, int[]> typeToIDArray)
  {
    this.typeToIDArray = typeToIDArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Compute a sketch from a database's records: the IDs of each type's records,
   * leaving out the root records that every database has and that are never saved.
   *
   * @param records The records of the database, as loaded from its XML files
   * @return A new DatabaseSketch
   */
  public static DatabaseSketch compute(Stream<? extends HDT_Record> records)
  {
    EnumMap<RecordType, List<Integer>> typeToIDList = new EnumMap<>(RecordType.class);

    records.filter(record -> isUnstoredRecord(record) == false)
           .forEach(record -> typeToIDList.computeIfAbsent(record.getType(), _ -> new ArrayList<>()).add(record.getID()));

    EnumMap<RecordType, int[]> typeToIDArray = new EnumMap<>(RecordType.class);

    typeToIDList.forEach((type, idList) ->
    {
      int[] arr = idList.stream().mapToInt(Integer::intValue).sorted().toArray();
      typeToIDArray.put(type, arr);
    });

    return new DatabaseSketch(typeToIDArray);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Compute weighted Jaccard similarity between this sketch and another
   * and return whether the similarity is above the threshold.
   * Weights are proportional to the union size of each record type.
   *
   * @param other The other sketch to compare against
   * @return Whether similarity in [0.0, 1.0] is above the threshold
   */
  public boolean isVerySimilarTo(DatabaseSketch other)
  {
    Set<RecordType> allTypes = EnumSet.noneOf(RecordType.class);
    allTypes.addAll(      typeToIDArray.keySet());
    allTypes.addAll(other.typeToIDArray.keySet());

    if (allTypes.isEmpty()) return true;

    double weightedSum = 0.0, totalWeight = 0.0;

    for (RecordType type : allTypes)
    {
      int[] a =       typeToIDArray.getOrDefault(type, new int[0]),
            b = other.typeToIDArray.getOrDefault(type, new int[0]);

      int intersectionSize = sortedIntersectionSize(a, b),
          unionSize = (a.length + b.length) - intersectionSize;

      if (unionSize == 0) continue;

      double jaccard = (double) intersectionSize / unionSize;
      weightedSum += jaccard * unionSize;
      totalWeight += unionSize;
    }

    return (totalWeight == 0.0 ? 1.0 : weightedSum / totalWeight) >= SIMILARITY_THRESHOLD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Count the intersection size of two sorted int arrays.
   */
  private static int sortedIntersectionSize(int[] a, int[] b)
  {
    int ndxA = 0, ndxB = 0, count = 0;

    while ((ndxA < a.length) && (ndxB < b.length))
    {
      if (a[ndxA] == b[ndxB])
      {
        count++;
        ndxA++;
        ndxB++;
      }
      else if (a[ndxA] < b[ndxB])
        ndxA++;
      else
        ndxB++;
    }

    return count;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Serialize this sketch to a JSON file.
   */
  public void writeTo(FilePath file)
  {
    JsonObj totalObj = new JsonObj();
    totalObj.put("version", (long) CURRENT_SKETCH_FORMAT_VERSION);

    JsonObj typeToIDsObj = new JsonObj();

    typeToIDArray.forEach((type, idArray) ->
    {
      JsonArray jsonArr = new JsonArray();

      for (int id : idArray)
        jsonArr.add((long) id);

      typeToIDsObj.put(getTypeTagStr(type), jsonArr);
    });

    totalObj.put("typeToIDArray", typeToIDsObj);

    try (Writer writer = new OutputStreamWriter(new FileOutputStream(file.toFile()), StandardCharsets.UTF_8))
    {
      writer.write(totalObj.toString());
    }
    catch (IOException e) { logThrowable(e); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Deserialize a sketch from a JSON file.
   * @return The deserialized sketch, or null if the file cannot be read
   */
  public static DatabaseSketch readFrom(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath) || (filePath.exists() == false)) return null;

    try (Reader reader = new InputStreamReader(new FileInputStream(filePath.toFile()), StandardCharsets.UTF_8))
    {
      JsonObj totalObj = JsonObj.parseJsonObj(reader),
              typeToIDsObj = totalObj.getObj("typeToIDArray");

      if ((totalObj.getLong("version", -1) != CURRENT_SKETCH_FORMAT_VERSION) || (typeToIDsObj == null)) return null;

      EnumMap<RecordType, int[]> typeToIDArray = new EnumMap<>(RecordType.class);

      for (String typeStr : typeToIDsObj.keySet())
      {
        RecordType type = parseTypeTagStr(typeStr);
        if (type == hdtNone) continue;

        JsonArray jsonArr = typeToIDsObj.getArray(typeStr);
        if (jsonArr == null) continue;

        int[] idArray = IntStream.range(0, jsonArr.size())
                                 .map(ndx -> parseInt(jsonArr.getLongAsStrSafe(ndx), -1))
                                 .toArray();

        typeToIDArray.put(type, idArray);
      }

      return new DatabaseSketch(typeToIDArray);
    }
    catch (IOException | ParseException e)
    {
      logThrowable(e);
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
