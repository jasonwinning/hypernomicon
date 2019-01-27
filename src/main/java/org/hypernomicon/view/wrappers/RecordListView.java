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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.*;
import org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.view.wrappers.HyperTable.HyperMenuItem;

public interface RecordListView
{
  @FunctionalInterface interface RecordHandler<HDT_T extends HDT_Base>     { void handle(HDT_T record); }
  @FunctionalInterface interface CondRecordHandler<HDT_T extends HDT_Base> { boolean handle(HDT_T record); }

  <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addContextMenuItem    (String caption, Class<HDT_T> klass, RecordHandler<HDT_T> handler);

  <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addCondContextMenuItem(String caption, Class<HDT_T> klass,
                                                                       CondRecordHandler<HDT_T> condHandler, RecordHandler<HDT_T> handler);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addDefaultMenuItems(RecordListView rlv)
  {
    rlv.addCondContextMenuItem("Launch work file", HDT_Work.class,
        HDT_Work::canLaunch,
        work -> work.launch(-1));

    rlv.addCondContextMenuItem("Show in Preview Window", HDT_Work.class,
        HDT_Work::canLaunch,
        work ->
        {
          PreviewSource src = ui.determinePreviewContext();
          previewWindow.setPreview(src, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
          ui.openPreviewWindow(src);
        });

    rlv.addCondContextMenuItem("Launch", HDT_WorkFile.class,
        workFile -> workFile.getPath().isEmpty() == false,
        workFile -> launchFile(workFile.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in Preview Window", HDT_WorkFile.class,
        workFile -> workFile.getPath().isEmpty() == false,
        workFile ->
        {
          PreviewSource src = ui.determinePreviewContext();
          previewWindow.setPreview(src, workFile.getPath().getFilePath(), -1, -1, workFile);
          ui.openPreviewWindow(src);
        });

    rlv.addCondContextMenuItem("Launch file", HDT_MiscFile.class,
        miscFile -> miscFile.getPath().isEmpty() == false,
        miscFile ->
        {
          miscFile.viewNow();
          launchFile(miscFile.getPath().getFilePath());
        });

    rlv.addCondContextMenuItem("Show in Preview Window", HDT_MiscFile.class,
        miscFile -> miscFile.getPath().isEmpty() == false,
        miscFile ->
        {
          PreviewSource src = ui.determinePreviewContext();
          previewWindow.setPreview(src, miscFile.getPath().getFilePath(), -1, -1, miscFile);
          ui.openPreviewWindow(src);
        });

    rlv.addCondContextMenuItem("Show in File Manager", HDT_WorkFile.class,
        workFile -> workFile.getPath().isEmpty() == false,
        workFile -> ui.goToFileInManager(workFile.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in File Manager", HDT_MiscFile.class,
        miscFile -> miscFile.getPath().isEmpty() == false,
        miscFile -> ui.goToFileInManager(miscFile.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in File Manager", HDT_Folder.class,
        folder -> folder.getPath().isEmpty() == false,
        folder -> ui.goToFileInManager(folder.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in system explorer", HDT_MiscFile.class,
        miscFile -> miscFile.getPath().isEmpty() == false,
        miscFile -> highlightFileInExplorer(miscFile.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in system explorer", HDT_WorkFile.class,
        workFile -> workFile.getPath().isEmpty() == false,
        workFile -> highlightFileInExplorer(workFile.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show in system explorer", HDT_Folder.class,
        folder -> folder.getPath().isEmpty() == false,
        folder -> highlightFileInExplorer(folder.getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show folder in File Manager", HDT_Note.class,
        note -> nullSwitch(note.folder.get(), false, folder -> folder.getPath().isEmpty() == false),
        note -> ui.goToFileInManager(note.folder.get().getPath().getFilePath()));

    rlv.addCondContextMenuItem("Show folder in system explorer", HDT_Note.class,
        note -> nullSwitch(note.folder.get(), false, folder -> folder.getPath().isEmpty() == false),
        note -> highlightFileInExplorer(note.folder.get().getPath().getFilePath()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
