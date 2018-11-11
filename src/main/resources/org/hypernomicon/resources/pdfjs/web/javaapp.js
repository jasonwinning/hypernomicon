/*
 * Copyright 2017-2018 Jason Winning
 * 
 * This file is new with Hypernomicon and is not part of PDF.js
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

function openPdfFile(fileStr, pageNum, sidebarView) {
  
  if (PDFViewerApplication.initialized == false) {
    window.setTimeout(openPdfFile, 50, fileStr, pageNum, sidebarView);
    return;
  }
  
  PDFViewerApplicationOptions.set('initialPage', pageNum);
  PDFViewerApplicationOptions.set('sidebarViewOnLoad', sidebarView);
  PDFViewerApplicationOptions.set('disablePageMode', true);
  PDFViewerApplicationOptions.set('showPreviousViewOnLoad', false);
  
  PDFViewerApplication.pdfViewer.eventBus.on('pagechange', function (e) { javaApp.pageChange(e.pageNumber); });
  PDFViewerApplication.pdfViewer.eventBus.on('sidebarviewchanged', function (e) { javaApp.sidebarChange(e.view); });  
  
  PDFViewerApplication.open(fileStr).then(function() {
    javaApp.openDone(true, { });
  }, function (error) {
    javaApp.openDone(false, error);
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function closePdfFile() {
	PDFViewerApplication.close().then(function () {
	  javaApp.closeDone(true, { });
	}, function (error) {
	  javaApp.closeDone(false, error);
	});
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function getPdfData() {
  var pagesCount = PDFViewerApplication.pagesCount;
  var pagesLeft = pagesCount;
  var params = { intent: 'display' };
  var annotPages = [];
  var pdfDocument = PDFViewerApplication.pdfDocument;
     
  pdfDocument.getPageLabels().then(function (pageLabels) {
    for (var pageNum = 1; pageNum <= pagesCount; ++pageNum) {
      pdfDocument.getPage(pageNum).then(function (pageNum, pdfPage) {
        pdfPage.getAnnotations(params).then(function (pageNum, annotations) {
          for (var ndx = 0; ndx < annotations.length; ndx++) {
            var subtype = annotations[ndx].subtype;
            if ((subtype !== "Link") && (subtype !== "Widget")) {
              if (annotPages.indexOf(pageNum) === -1) {
                annotPages.push(pageNum);
              }
            }
          }
          pagesLeft--;
          if (pagesLeft === 0) {
            javaApp.setData({ annotPages, pageLabels });
          }
        }.bind(null, pageNum));
      }.bind(null, pageNum));
    }
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
