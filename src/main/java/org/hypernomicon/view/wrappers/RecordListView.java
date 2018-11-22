/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.view.wrappers.HyperTable.HyperMenuItem;

public interface RecordListView
{
  @FunctionalInterface interface RecordHandler     { public void handle(HDT_Base record); }
  @FunctionalInterface interface CondRecordHandler { public boolean handle(HDT_Base record); }
  
  public HyperMenuItem addContextMenuItem    (HDT_RecordType recordType, String caption, RecordListView.RecordHandler handler);
  public HyperMenuItem addCondContextMenuItem(HDT_RecordType recordType, String caption, RecordListView.CondRecordHandler condHandler, RecordListView.RecordHandler handler);
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addDefaultMenuItems(RecordListView rlv)
  {
    rlv.addCondContextMenuItem(hdtWork, "Launch work file", 
        record -> HDT_Work.class.cast(record).canLaunch(),
        record -> HDT_Work.class.cast(record).launch(-1));
    
    rlv.addCondContextMenuItem(hdtWork, "Show in Preview Window", 
        record -> HDT_Work.class.cast(record).canLaunch(),
        record -> 
        {
          HDT_Work work = HDT_Work.class.cast(record);
          
          PreviewSource src = ui.determinePreviewContext();          
          previewWindow.setPreview(src, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
          ui.openPreviewWindow(src);
        });
    
    rlv.addCondContextMenuItem(hdtWorkFile, "Launch", 
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> launchFile(HDT_WorkFile.class.cast(record).getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtWorkFile, "Show in Preview Window", 
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> 
        {
          HDT_WorkFile workFile = HDT_WorkFile.class.cast(record);
          
          PreviewSource src = ui.determinePreviewContext(); 
          previewWindow.setPreview(src, workFile.getPath().getFilePath(), -1, -1, workFile);
          ui.openPreviewWindow(src);
        });
    
    rlv.addCondContextMenuItem(hdtMiscFile, "Launch file", 
        record -> HDT_MiscFile.class.cast(record).getPath().isEmpty() == false,
        record ->
        {
          HDT_MiscFile miscFile = HDT_MiscFile.class.cast(record);
          miscFile.viewNow();
          launchFile(miscFile.getPath().getFilePath());
        });
    
    rlv.addCondContextMenuItem(hdtMiscFile, "Show in Preview Window", 
        record -> HDT_MiscFile.class.cast(record).getPath().isEmpty() == false,
        record -> 
        {
          HDT_MiscFile miscFile = HDT_MiscFile.class.cast(record);
          
          PreviewSource src = ui.determinePreviewContext(); 
          previewWindow.setPreview(src, miscFile.getPath().getFilePath(), -1, -1, miscFile);
          ui.openPreviewWindow(src);
        });

    rlv.addCondContextMenuItem(hdtWorkFile, "Show in File Manager", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> ui.goToFileInManager(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtMiscFile, "Show in File Manager", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> ui.goToFileInManager(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));
    
    rlv.addCondContextMenuItem(hdtFolder, "Show in File Manager", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> ui.goToFileInManager(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));
    
    rlv.addCondContextMenuItem(hdtMiscFile, "Show in system explorer", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> highlightFileInExplorer(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtWorkFile, "Show in system explorer", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> highlightFileInExplorer(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtFolder, "Show in system explorer", 
        record -> HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false,
        record -> highlightFileInExplorer(HDT_RecordWithPath.class.cast(record).getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtNote, "Show folder in File Manager",
        record -> nullSwitch(HDT_Note.class.cast(record), false, note -> note.folder.get().getPath().isEmpty() == false), 
        record -> ui.goToFileInManager(HDT_Note.class.cast(record).folder.get().getPath().getFilePath()));

    rlv.addCondContextMenuItem(hdtNote, "Show folder in system explorer",
        record -> nullSwitch(HDT_Note.class.cast(record), false, note -> note.folder.get().getPath().isEmpty() == false),
        record -> highlightFileInExplorer(HDT_Note.class.cast(record).folder.get().getPath().getFilePath()));
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
