/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.util;

import org.hypernomicon.App;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.WindowStack;
import org.hypernomicon.view.settings.LaunchCommandsDlgCtrlr;
import org.hypernomicon.view.dialogs.InternetCheckDlgCtrlr;
import org.hypernomicon.view.dialogs.LockedDlgCtrlr;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static javax.xml.bind.DatatypeConverter.printBase64Binary;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.lang.reflect.Constructor;

import static java.nio.charset.StandardCharsets.*;
import static java.util.Collections.binarySearch;

import java.net.InetAddress;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.security.SecureRandom;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.text.NumberFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.jar.Manifest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.swing.filechooser.FileSystemView;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.beans.property.DoubleProperty;
import javafx.event.EventTarget;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.image.ImageView;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Alert;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Control;
import javafx.scene.control.DialogPane;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.util.Duration;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.mime.MediaType;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import com.google.common.escape.Escaper;
import com.google.common.html.HtmlEscapers;
import com.ibm.icu.text.CharsetDetector;
import com.ibm.icu.text.Transliterator;

import javafx.scene.control.skin.ComboBoxListViewSkin;
import com.teamdev.jxbrowser.chromium.internal.Environment;

//---------------------------------------------------------------------------

public final class Util
{
  public static final JSONParser jsonParser = new JSONParser();

  public static final StopWatch stopWatch1 = new StopWatch(), stopWatch2 = new StopWatch(), stopWatch3 = new StopWatch(),
                                stopWatch4 = new StopWatch(), stopWatch5 = new StopWatch(), stopWatch6 = new StopWatch();

  public static final Escaper htmlEscaper = HtmlEscapers.htmlEscaper();

  static String hostName = "";

//---------------------------------------------------------------------------

  public static void setDividerPosition(SplitPane sp, String key, int ndx)
  {
    double pos = appPrefs.getDouble(key, -1.0);

    if (pos >= 0)
      sp.setDividerPosition(ndx, pos);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void getDividerPosition(SplitPane sp, String key, int ndx)
  {
    appPrefs.putDouble(key, sp.getDividerPositions()[ndx]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void copyToClipboard(String str)
  {
    ClipboardContent content = new ClipboardContent();
    content.putString(str);
    Clipboard.getSystemClipboard().setContent(content);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getClipboardText(boolean noCarriageReturns)
  {
    String text = safeStr((String) Clipboard.getSystemClipboard().getContent(DataFormat.PLAIN_TEXT));
    if (text.isEmpty()) return "";

    text = text.replace("\ufffd", ""); // I don't know what this is but it is usually appended at the end when copying text from Acrobat

    if (noCarriageReturns)
      text = convertToSingleLine(text);

    return text;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String convertToSingleLine(String text)
  {
    return text.replace("\ufffd", "")  // I don't know what this is but it is usually appended at the end when copying text from Acrobat

      .replaceAll("\\R+(\\R)", "$1")
      .replaceAll("(\\R)\\R+", "$1")

      .replaceAll("(\\v)\\v+", "$1")
      .replaceAll("\\v+(\\v)", "$1")

      .replaceAll("\\R+(\\h)", "$1")
      .replaceAll("(\\h)\\R+", "$1")

      .replaceAll("\\v+(\\h)", "$1")
      .replaceAll("(\\h)\\v+", "$1")

      .replaceAll("\\R+", " ")
      .replaceAll("\\v+", " ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String trimLines(String input)
  {
    input = safeStr(input);

    if (ultraTrim(convertToSingleLine(input)).isEmpty()) return "";

    List<String> list = convertMultiLineStrToStrList(input, true);

    list.replaceAll(Util::ultraTrim);

    return strListToStr(list, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String mainTextHeadStyleTag()
  {
    return "<style>p { margin-top: 0em; margin-bottom: 0em; } " +
           "body { font-family: arial; font-size: 10pt; } </style>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int parseInt(String value, int def)
  {
    try { return Integer.parseInt(value); }
    catch (NumberFormatException nfe) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static long parseLong(String value, long def)
  {
    try { return Long.parseLong(value); }
    catch (NumberFormatException nfe) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum MessageDialogType
  {
    mtWarning,
    mtError,
    mtInformation
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void removeDupsInStrList(List<String> list)
  {
    Set<String> set = new HashSet<>();
    Iterator<String> it = list.iterator();

    while (it.hasNext())
    {
      String str = it.next();
      if (set.contains(str))
        it.remove();
      else
        set.add(str);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean strListsEqual(List<String> list1, List<String> list2, boolean ignoreCase)
  {
    if ((list1 == null) != (list2 == null)) return false;
    if ((list1 == null) && (list2 == null)) return true;
    if (list1.size() != list2.size()) return false;

    for (int ndx = 0; ndx < list1.size(); ndx++)
    {
      String str1 = ultraTrim(list1.get(ndx)), str2 = ultraTrim(list2.get(ndx));

      if ((ignoreCase ? str1.equalsIgnoreCase(str2) : str1.equals(str2)) == false)
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchORCID(String orcid, String first, String last)
  {
    if (orcid.length() > 0)
      openWebLink("http://orcid.org/" + escapeURL(orcid, false));
    else if ((first + last).length() > 0)
      openWebLink("https://orcid.org/orcid-search/quick-search/?searchQuery=" + escapeURL(last + ", " + first, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String removeAllParentheticals(String str)
  {
    while (str.contains("("))
      str = removeFirstParenthetical(str);

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String removeFirstParenthetical(String str)
  {
    int pos1 = str.indexOf('('), pos2 = str.indexOf(')');

    if (pos1 >= 0)
    {
      String result = str.substring(0, pos1).trim();
      if (pos2 > pos1)
        result = String.valueOf(result + " " + safeSubstring(str, pos2 + 1, str.length()).trim()).trim();

      return result;
    }
    else
      return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String escapeURL(String url, boolean removeParen)
  {
    if (removeParen)
      url = removeFirstParenthetical(url);

    return URLEncoder.encode(url.trim(), UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String unescapeURL(String url)
  {
    try
    {
      return URLDecoder.decode(safeStr(url), UTF_8);
    }
    catch (IllegalArgumentException e)
    {
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchWorldCatISBN(String isbn)
  {
    List<String> list = matchISBN(isbn);
    if (collEmpty(list) == false)
      openWebLink("http://www.worldcat.org/search?q=bn%3A" + list.get(0) + "&qt=advanced");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchDOI(String str)
  {
    String doi = matchDOI(str);
    if (doi.length() > 0)
      openWebLink("http://dx.doi.org/" + escapeURL(doi, false));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static void openWebLink(String url)
  {
    url = url.trim();

    if (url.isEmpty()) return;

    if (url.indexOf(":") == -1)
      url = "http://" + url;

    DesktopApi.browse(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launchFile(FilePath filePath)
  {
    launchWorkFile(filePath, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launchWorkFile(FilePath filePath, int pageNum)
  {
    if (FilePath.isEmpty(filePath)) return;

    String readerPath = appPrefs.get(PREF_KEY_PDF_READER, "");

    if ((filePath.getExtensionOnly().toLowerCase().equals("pdf") == false) || readerPath.isEmpty())
    {
      DesktopApi.open(filePath);
      return;
    }

    if (pageNum < 1) pageNum = 1;

    LaunchCommandsDlgCtrlr.launch(readerPath, filePath, PREF_KEY_PDF_READER_COMMANDS, PREF_KEY_PDF_READER_COMMAND_TYPE, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void highlightFileInExplorer(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    try
    {
      if (SystemUtils.IS_OS_WINDOWS)
        Runtime.getRuntime().exec("explorer.exe /select,\"" + filePath + "\"").waitFor();

      else if (SystemUtils.IS_OS_MAC)
        Runtime.getRuntime().exec(new String[] {"open", "-R", filePath.toString()}).waitFor();

      else if (SystemUtils.IS_OS_LINUX)
      {
        if (DesktopApi.exec(false, false, new StringBuilder(), "nautilus", filePath.toString()) == false)
          launchFile(filePath.getDirOnly());  // this won't highlight the file in the folder
      }

      // xdg-mime query default inode/directory

      else
        launchFile(filePath.getDirOnly());  // this won't highlight the file in the folder
    }
    catch (Exception e)
    {
      messageDialog("An error occurred while trying to show the file: " + filePath + ". " + e.getMessage(), mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult abortRetryIgnoreDialog(String msg)
  {
    PopupDialog dlg = new PopupDialog(msg);

    dlg.addButton("Abort" , mrAbort);
    dlg.addButton("Retry" , mrRetry);
    dlg.addButton("Ignore", mrIgnore);

    return dlg.showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult yesNoCancelDialog(String msg)
  {
    PopupDialog dlg = new PopupDialog(msg);

    dlg.addButton("Yes"   , mrYes);
    dlg.addButton("No"    , mrNo);
    dlg.addButton("Cancel", mrCancel);

    return dlg.showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult seriesConfirmDialog(String msg)
  {
    PopupDialog dlg = new PopupDialog(msg);

    dlg.addButton("Yes"       , mrYes);
    dlg.addButton("No"        , mrNo);
    dlg.addButton("Yes to all", mrYesToAll);
    dlg.addButton("No to all" , mrNoToAll);

    return dlg.showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean confirmDialog(String msg)
  {
    PopupDialog dlg = new PopupDialog(msg);

    dlg.addButton("Yes", mrYes);
    dlg.addButton("No" , mrNo);

    return dlg.showModal() == mrYes;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean falseWithErrorMessage  (String msg                  ) { return falseWithMessage(msg, mtError      , null       ); }
  public static boolean falseWithErrorMessage  (String msg, Node nodeToFocus) { return falseWithMessage(msg, mtError      , nodeToFocus); }
  public static boolean falseWithWarningMessage(String msg                  ) { return falseWithMessage(msg, mtWarning    , null       ); }
  public static boolean falseWithWarningMessage(String msg, Node nodeToFocus) { return falseWithMessage(msg, mtWarning    , nodeToFocus); }
  public static boolean falseWithInfoMessage   (String msg                  ) { return falseWithMessage(msg, mtInformation, null       ); }
  public static boolean falseWithInfoMessage   (String msg, Node nodeToFocus) { return falseWithMessage(msg, mtInformation, nodeToFocus); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean falseWithMessage(String msg, MessageDialogType mt, Node nodeToFocus)
  {
    messageDialog(msg, mt);
    if (nodeToFocus != null) safeFocus(nodeToFocus);
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void messageDialog(String msg, MessageDialogType mt)
  {
    messageDialog(msg, mt, false);
  }

  public static void messageDialog(String msg, MessageDialogType mt, boolean wait)
  {
    if (wait) messageDialogShowing = true;

    runInFXThread(() ->
    {
      Alert alert = null;

      messageDialogShowing = true;

      switch (mt)
      {
        case mtWarning :
          alert = new Alert(AlertType.WARNING);
          alert.setHeaderText("Warning");
          break;

        case mtError :
          alert = new Alert(AlertType.ERROR);
          alert.setHeaderText("Error");
          break;

        case mtInformation :
          alert = new Alert(AlertType.INFORMATION);
          alert.setHeaderText("Information");
          break;

        default:

          return;
      }

      alert.setTitle(appTitle);
      alert.setContentText(msg);

      showAndWait(alert);
      messageDialogShowing = false;
    });

    while (wait && messageDialogShowing)
      sleepForMillis(50);
  }

  private static boolean messageDialogShowing = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void showStackTrace(Throwable e)
  {
    LockedDlgCtrlr.create("Error", e).showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ButtonType showAndWait(Alert dlg)
  {
    WindowStack windowStack = ui == null ? null : ui.windows;

    if (windowStack != null)
      windowStack.push(dlg);

    if (SystemUtils.IS_OS_LINUX)
    {
      DialogPane dlgPane = dlg.getDialogPane();

      dlgPane.setMinSize (800, 400);
      dlgPane.setMaxSize (800, 400);
      dlgPane.setPrefSize(800, 400);
    }

    if (windowStack != null)
    {
      Stage owner = windowStack.getOutermostStage();
      if ((owner != null) && (owner.isShowing() == false))
        owner = null;

      dlg.initOwner(owner);
    }

    Optional<ButtonType> result = dlg.showAndWait();

    if (windowStack != null)
      windowStack.pop();

    return result.orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean parseBoolean(String s)
  {
    if (Boolean.parseBoolean(s)) return true;

    s = s.trim().toLowerCase();

    if (s.equalsIgnoreCase(Boolean.TRUE .toString().trim())) return true;
    if (s.equalsIgnoreCase(Boolean.FALSE.toString().trim())) return false;

    return (s.indexOf("yes") == 0) || (s.indexOf("tru") == 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final DateTimeFormatter

   userReadableDateTimeFormatter = DateTimeFormatter.ofPattern("M/d/yyyy h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),
   userReadableTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   httpDate = DateTimeFormatter.RFC_1123_DATE_TIME.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   iso8601Format = DateTimeFormatter.ISO_INSTANT.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),
   iso8601FormatOffset = DateTimeFormatter.ISO_OFFSET_DATE_TIME.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault());

  public static final NumberFormat numberFormat = NumberFormat.getInstance();

  public static final Instant APP_GENESIS_INSTANT = parseIso8601("2012-08-03T06:00:00Z");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String dateTimeToIso8601(TemporalAccessor t) { return iso8601Format.format(t); }
  public static Instant parseIso8601(String s)               { return Instant.from(iso8601Format.parse(s)); }

  public static String timeToUserReadableStr(TemporalAccessor t)     { return userReadableTimeFormatter.format(t); }
  public static String dateTimeToUserReadableStr(TemporalAccessor t) { return userReadableDateTimeFormatter.format(t); }

  public static String dateTimeToIso8601offset(TemporalAccessor t) { return iso8601FormatOffset.format(t); }
  public static Instant parseIso8601offset(String s)               { return Instant.from(iso8601FormatOffset.parse(s)); }

  public static String dateTimeToHttpDate(TemporalAccessor t) { return httpDate.format(t); }
  public static Instant parseHttpDate(String s)               { return Instant.from(httpDate.parse(s)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void saveStringBuilderToFile(StringBuilder sb, FilePath filePath) throws IOException
  {
    int bufLen = 65536;
    char[] charArray = new char[bufLen];

    try (OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(filePath.toFile()), UTF_8))
    {
      for (int offsetIntoSB = 0; offsetIntoSB < sb.length(); offsetIntoSB += bufLen)
      {
        bufLen = Math.min(bufLen, sb.length() - offsetIntoSB);
        sb.getChars(offsetIntoSB, offsetIntoSB + bufLen, charArray, 0);
        writer.write(charArray, 0, bufLen);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String safeSubstring(String str, int start, int end)
  {
    if (str == null) return "";

    if (start < 0) start = 0;

    if (start >= str.length()) return "";

    if (end < start) end = start;

    if (end > str.length()) return str.substring(start);

    return str.substring(start, end);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Removes all horizontal whitespace characters [ \t\xA0\u1680\u180e\u2000-\u200a\u202f\u205f\u3000] at the beginning and end of the string

  public static String ultraTrim(String text)
  {
    return text.replaceAll("(^\\h+)|(\\h+$)", "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int indexOfAny(String chars, String text)
  {
    int lowestPos = -1, curPos;

    for (char c : chars.toCharArray())
    {
      curPos = text.indexOf(c);

      if ((curPos > -1) && ((lowestPos == -1) || (curPos < lowestPos)))
        lowestPos = curPos;
    }

    return lowestPos;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getImageDataURI(String relativePath)
  {
    try (BufferedInputStream stream = new BufferedInputStream(App.class.getResourceAsStream(relativePath));)
    {
      byte[] array = new byte[stream.available()];

      stream.read(array);

      return "data:image/png;base64," + printBase64Binary(array);
    }
    catch (Exception e)
    {
      messageDialog("Error: " + e.getMessage(), mtError);
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // copies node1 to node2

  public static void copyRegionLayout(Region node1, Region node2)
  {
    setAnchors(node2, AnchorPane.getTopAnchor (node1), AnchorPane.getBottomAnchor(node1),
                      AnchorPane.getLeftAnchor(node1), AnchorPane.getRightAnchor (node1));

    GridPane.setColumnIndex(node2, GridPane.getColumnIndex(node1));
    GridPane.setColumnSpan (node2, GridPane.getColumnSpan (node1));
    GridPane.setRowIndex   (node2, GridPane.getRowIndex   (node1));
    GridPane.setRowSpan    (node2, GridPane.getRowSpan    (node1));

    node2.setLayoutX(node1.getLayoutX());
    node2.setLayoutY(node1.getLayoutY());

    node2.setMinSize (node1.getMinWidth (), node1.getMinHeight ());
    node2.setMaxSize (node1.getMaxWidth (), node1.getMaxHeight ());
    node2.setPrefSize(node1.getPrefWidth(), node1.getPrefHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setAnchors(Node node, Double top, Double bottom, Double left, Double right)
  {
    AnchorPane.setTopAnchor   (node, top   );
    AnchorPane.setBottomAnchor(node, bottom);
    AnchorPane.setLeftAnchor  (node, left  );
    AnchorPane.setRightAnchor (node, right );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void safeFocus(Node node)
  {
    if (node.isDisabled() == false)
      runInFXThread(node::requestFocus);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void scaleNodeForDPI(Node node)
  {
    boolean childrenOnly = false;

    if (node == null) return;

    if (node.getId() != null)
    {
      if (node.getId().equals("noScale"))
        return;

      if (node.getId().equals("childrenOnly"))
        childrenOnly = true;
    }

    if (childrenOnly == false)
    {
      if (node instanceof Region)
      {
        Region region = Region.class.cast(node);

        scalePropertiesForDPI(region.prefHeightProperty(), region.prefWidthProperty(),
                              region.maxHeightProperty() , region.maxWidthProperty(),
                              region.minHeightProperty() , region.minWidthProperty());
      }

      if (((node instanceof javafx.scene.shape.Path) == false) &&
          ((node instanceof javafx.scene.text.Text) == false))
      {
        scalePropertiesForDPI(node.layoutXProperty(), node.layoutYProperty());
      }

      Double val = AnchorPane.getBottomAnchor(node);
      if ((val != null) && (val.doubleValue() > 0.0))
        AnchorPane.setBottomAnchor(node, val.doubleValue() * displayScale);

      val = AnchorPane.getTopAnchor(node);
      if ((val != null) && (val.doubleValue() > 0.0))
        AnchorPane.setTopAnchor(node, val.doubleValue() * displayScale);

      val = AnchorPane.getLeftAnchor(node);
      if ((val != null) && (val.doubleValue() > 0.0))
        AnchorPane.setLeftAnchor(node, val.doubleValue() * displayScale);

      val = AnchorPane.getRightAnchor(node);
      if ((val != null) && (val.doubleValue() > 0.0))
        AnchorPane.setRightAnchor(node, val.doubleValue() * displayScale);
    }

    if (node instanceof TableView) ((TableView<?>)node).getColumns().forEach(column ->
      scalePropertiesForDPI(column.maxWidthProperty(), column.minWidthProperty(), column.prefWidthProperty()));

    if (node instanceof GridPane)
    {
      GridPane gridPane = GridPane.class.cast(node);

      gridPane.getColumnConstraints().forEach(cc -> scalePropertiesForDPI(cc.maxWidthProperty(), cc.minWidthProperty(), cc.prefWidthProperty()));
      gridPane.getRowConstraints().forEach(rc -> scalePropertiesForDPI(rc.maxHeightProperty(), rc.minHeightProperty(), rc.prefHeightProperty()));
    }

    if (node instanceof ToolBar)
      ToolBar.class.cast(node).getItems().forEach(Util::scaleNodeForDPI);
    else if (node instanceof TitledPane)
      scaleNodeForDPI(TitledPane.class.cast(node).getContent());
    else if (node instanceof TabPane)
      TabPane.class.cast(node).getTabs().forEach(tab -> scaleNodeForDPI(tab.getContent()));
    else if (node instanceof Parent)
      Parent.class.cast(node).getChildrenUnmodifiable().forEach(Util::scaleNodeForDPI);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void scalePropertiesForDPI(DoubleProperty... props)
  {
    double[] vals = new double[props.length];

    for (int ndx = 0; ndx < props.length; ndx++)
      vals[ndx] = props[ndx].get();

    for (int ndx = 0; ndx < props.length; ndx++)
    {
      if (vals[ndx] > 0.0)
      {
        vals[ndx] = vals[ndx] * displayScale;
        props[ndx].set(vals[ndx]);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String abbreviate(String text)
  {
    text = safeStr(text);

    if (text.length() < 35) return text;

    return StringUtils.stripEnd(text.substring(0, 35), " .") + "...";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String camelToTitle(String in)
  {
    String out = "";

    for (int ndx = 0; ndx < in.length(); ndx++)
    {
      char c = in.charAt(ndx);
      out = out + (Character.isUpperCase(c) ? " " + c : c);
    }

    return titleCase(out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String sentenceCase(String str)
  {
    if (safeStr(str).isEmpty()) return "";

    str = str.toLowerCase();

    Pattern p = Pattern.compile("([\\.:?!]\\h)(\\p{IsAlphabetic})");

    str = p.matcher(str).replaceAll(match -> match.group(1) + match.group(2).toUpperCase());

    return str.substring(0, 1).toUpperCase() + str.substring(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String titleCase(String str)
  {
    MutableInt pos = new MutableInt(0);

    while (str.matches(".*\\h[.,:;)].*"))
      str = str.replaceFirst("\\h([.,:;)])", "$1");   // remove space before character

    while (str.matches(".*[(]\\h.*"))
      str = str.replaceFirst("([(])\\h", "$1");   // remove space after character

    for (String word = getNextWord(str, pos); word.length() > 0; word = getNextWord(str, pos))
    {
      int end = pos.intValue(), start = end - word.length();

      String pre = "", post = "";
      char lastChar = ' ';
      boolean noCaps = false, endsWithDot = false;

      if (start > 0)
      {
        pre = str.substring(0, start).trim();
        if (pre.length() > 0)
        {
          lastChar = pre.charAt(pre.length() - 1);

          if (convertToEnglishChars(String.valueOf(lastChar)).equals("'"))
            if (pre.length() > 1)
              if (pre.charAt(pre.length() - 2) != ' ') // don't capitalize letter immediately after apostrophe
                if (str.charAt(start - 1) != ' ')      // do capitalize letter after an apostrophe plus a space
                  noCaps = true;
        }

        pre = str.substring(0, start);
      }

      if (end < str.length())
      {
        post = str.substring(end);
        if (post.charAt(0) == '.')
          endsWithDot = true;
      }

      word = word.toLowerCase();

      if (noCaps == false)
      {
        if ((lastChar == ':') || (lastChar == '?') || (lastChar == '/'))
          word = word.substring(0, 1).toUpperCase() + safeSubstring(word, 1, word.length()).toLowerCase();
        else if (start == 0)
          word = word.substring(0, 1).toUpperCase() + safeSubstring(word, 1, word.length()).toLowerCase();
        else if ((word.length() == 1) && endsWithDot)
          word = word.substring(0, 1).toUpperCase();
        else
        {
          switch (word)
          {
            case "\u00e0": // Latin small letter a with grave accent, as in 'vis a vis' or 'a la'

            case "a"    : case "also" : case "amid" : case "an"  : case "and"  :
            case "as"   : case "at"   : case "atop" : case "but" : case "by"   :
            case "for"  : case "from" : case "if"   : case "in"  : case "into" :
            case "is"   : case "it"   : case "la"   : case "nor" : case "of"   :
            case "off"  : case "on"   : case "onto" : case "or"  : case "out"  :
            case "per"  : case "qua"  : case "sans" : case "so"  : case "than" :
            case "that" : case "the"  : case "then" : case "to"  : case "unto" :
            case "upon" : case "via"  : case "with" : case "yet" :
              break;

            default :
              word = word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase();
          }
        }
      }

      str = pre + word + post;
    }

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getNextWord(String str, MutableInt posObj)
  {
    int start = posObj.intValue(), end;
    boolean gotStart = false;

    while ((start < str.length()) && (gotStart == false))
    {
      if (Character.isAlphabetic(str.charAt(start)))
        gotStart = true;
      else
        start++;
    }

    if (gotStart == false) return "";

    for (end = start + 1; end < str.length(); end++)
    {
      if (Character.isAlphabetic(str.charAt(end)) == false)
      {
        posObj.setValue(end);
        return str.substring(start, end);
      }
    }

    posObj.setValue(end);
    return str.substring(start);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean checkInternetConnection()
  {
    return InternetCheckDlgCtrlr.create().checkInternet("https://www.google.com/");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static CloseableHttpClient getHTTPClient()
  {
    SSLContext sc = null;

    try
    {
      sc = SSLContext.getInstance("TLS");

      X509TrustManager trustMgr = new X509TrustManager()
      {
        @Override public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException { return; }
        @Override public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException { return; }
        @Override public X509Certificate[] getAcceptedIssuers()                                                        { return null; }
      };

      sc.init(null, new TrustManager[] { trustMgr }, new SecureRandom());
    }
    catch (Exception e)
    {
      throw new RuntimeException("Error while creating SSLContext", e);
    }

    return HttpClientBuilder.create().setSSLContext(sc).setSSLHostnameVerifier((hostname, session) -> true).build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void assignSB(StringBuilder sb, String s)
  {
    sb.replace(0, sb.length(), s);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getComputerName()
  {
    if (hostName.length() > 0) return hostName;

    hostName = safeStr(System.getenv("HOSTNAME"));
    if (hostName.length() > 0) return hostName;

    hostName = safeStr(System.getenv("COMPUTERNAME"));
    if (hostName.length() > 0) return hostName;

    try { hostName = safeStr(InetAddress.getLocalHost().getHostName()); }
    catch (UnknownHostException e) { return ""; }

    return hostName;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String safeStr(String s) { return s == null ? "" : s; }

  public static boolean collEmpty(Collection<?> c) { return c == null ? true : c.isEmpty(); }
  public static boolean collEmpty(Map<?, ?> m)     { return m == null ? true : m.isEmpty(); }

  public static <E> List<E> safeListOf(E e1)       { return e1 == null ? List.of() : List.of(e1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <E extends Enum<E>> E getEnumVal(int ord, Class<E> cls)
  {
    E[] vals = cls.getEnumConstants();

    if ((vals == null) || (ord < 0) || (ord > (vals.length - 1))) return null;

    return vals[ord];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String strListToStr(List<String> list, boolean emptiesOK)
  {
    return strListToStr(list, emptiesOK, false);
  }

  public static String strListToStr(List<String> list, boolean emptiesOK, boolean useSystemNewLineChar)
  {
    Stream<StringBuilder> strm = (emptiesOK ? list.stream() : list.stream().filter(one -> safeStr(one).length() > 0)).map(StringBuilder::new);
    return strm.reduce((all, one) -> all.append(useSystemNewLineChar ? System.lineSeparator() : "\n").append(one)).orElse(new StringBuilder()).toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String replaceChar(String str, int index, char replace)
  {
    if (str == null) return null;

    if ((index < 0) || (index >= str.length())) return str;

    char[] chars = str.toCharArray();
    chars[index] = replace;
    return String.valueOf(chars);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String NORMALIZE_ID = "NFD; [:Nonspacing Mark:] Remove; NFC";
  private static final Transliterator transliterator1 = Transliterator.getInstance("NFD; Any-Latin; NFC; " + NORMALIZE_ID),
                                      transliterator2 = Transliterator.getInstance("NFD; Latin-ASCII; NFC; " + NORMALIZE_ID);
  private static final Map<Character, String> charMap = new HashMap<>();

  public static String convertToEnglishChars(String input)
  {
    return convertToEnglishCharsWithMap(input, null);
  }

  public static String convertToEnglishCharsWithMap(String input, List<Integer> posMap)
  {
    StringBuilder output = new StringBuilder();

    if (posMap == null) posMap = new ArrayList<>();

    for (int inPos = 0; inPos < input.length(); inPos++)
    {
      char c = input.charAt(inPos);
      String s;

      if (c == '\u2014')
        s = String.valueOf(c);
      else
      {
        s = charMap.get(c);

        if (s == null)
        {
          s = transliterator2.transliterate(transliterator1.transliterate(String.valueOf(c)));

          charMap.put(c, s);
        }
      }

      output.append(s);

      for (int ndx = 0; ndx < s.length(); ndx++)
        posMap.add(inPos);
    }

    return output.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setFontSize(Node node)
  {
    double fontSize = appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize >= 1)
      node.setStyle("-fx-font-size: " + fontSize + "px;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String manifestValue(String key)
  {
    Class<App> theClass = App.class;
    String className = theClass.getSimpleName() + ".class",
           classPath = theClass.getResource(className).toString();

    if (!classPath.startsWith("jar")) return "";   // Class not from JAR

    String manifestPath = classPath.substring(0, classPath.lastIndexOf("!") + 1) + "/META-INF/MANIFEST.MF";

    try
    {
      Manifest manifest = new Manifest(new URL(manifestPath).openStream());
      return manifest.getMainAttributes().getValue(key);

    } catch (IOException e) { noOp(); }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The hacky nature of this function is due to the fact that the webview context menu is not publicly accessible
  // See https://bugs.openjdk.java.net/browse/JDK-8090931

  public static void setHTMLContextMenu(MenuItem... items)
  {
    Parent parent = nullSwitch(nullSwitch(findFirst(Window.getWindows(), window -> window instanceof ContextMenu),
                                          null, Window::getScene), null, Scene::getRoot);
    if (parent == null) return;

    List<Node> rootChildren = parent.getChildrenUnmodifiable();
    if (rootChildren.size() == 0) return;

    Node bridge = rootChildren.get(0).lookup(".context-menu");
    if (bridge == null) return;

    Class<? extends Object> contextMenuContentClass, menuItemContainerClass;

    try
    {
      contextMenuContentClass = Class.forName("com.sun.javafx.scene.control.ContextMenuContent");
      menuItemContainerClass = Class.forName("com.sun.javafx.scene.control.ContextMenuContent$MenuItemContainer");

      Node contextMenuContent = ((Parent)bridge).getChildrenUnmodifiable().get(0);
      Constructor<?> ctor = menuItemContainerClass.getDeclaredConstructor(contextMenuContentClass, MenuItem.class);

      List<Node> list = VBox.class.cast(contextMenuContentClass.getMethod("getItemsContainer").invoke(contextMenuContent)).getChildren();

      list.clear();
      for (MenuItem item : items)
        list.add((Node) ctor.newInstance(contextMenuContent, item));
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isStringUrl(String selText)
  {
    return (selText.indexOf("www.") > -1) || (selText.indexOf("http") > -1) ||
           (selText.indexOf(".com") > -1) || (selText.indexOf(".htm") > -1) ||
           (selText.indexOf(".org") > -1) || (selText.indexOf(".net") > -1) ||
           (selText.indexOf(".us")  > -1) || (selText.indexOf(".uk")  > -1) ||
           (selText.indexOf(".gov") > -1) || (selText.indexOf("://")  > -1) ||

           (selText.matches(".*\\w/\\w.*") && selText.matches(".*\\.[a-zA-Z].*"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <T> ListView<T> getCBListView(ComboBox<T> cb)
  {
    return nullSwitch((ComboBoxListViewSkin<T>)cb.getSkin(), null, skin -> (ListView<T>) skin.getPopupContent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This function is what seems to have mostly fixed the HTML editor bugs

  public static Parent removeFromParent(Node node)
  {
    Parent parent = node.getParent();

    if (parent instanceof Pane)
    {
      Pane pane = (Pane)parent;
      pane.getChildren().remove(node);
    }
    else if (parent instanceof ToolBar)
    {
      ToolBar toolBar = (ToolBar)parent;
      toolBar.getItems().remove(node);
    }

    return parent;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addToParent(Node child, Parent parent)
  {
    if (parent instanceof Pane)
    {
      Pane pane = (Pane)parent;

      if (pane.getChildren().contains(child) == false)
        pane.getChildren().add(child);
    }
    else if (parent instanceof ToolBar)
    {
      ToolBar toolBar = (ToolBar)parent;

      if (toolBar.getItems().contains(child) == false)
        toolBar.getItems().add(child);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This should never be changed. It is the same algorithm as String.hashCode() as of Java 8u112

  public static int stringHash(String value)
  {
    int h = 0, len = value.length();

    if (len == 0) return 0;

    char val[] = value.toCharArray();

    for (int i = 0; i < len; i++)
      h = 31 * h + val[i];

    return h;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runDelayedInFXThread(int cycles, int delayMS, Runnable runnable)
  {
    Timeline timeline = new Timeline();
    timeline.setCycleCount(cycles);

    timeline.getKeyFrames().add(new KeyFrame(Duration.millis(delayMS), event -> runnable.run()));
    timeline.play();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runOutsideFXThread(int delayMS, Runnable runnable)
  {
    new Timer(true).schedule(new TimerTask() { @Override public void run() { runnable.run(); }}, delayMS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runOutsideFXThread(Runnable runnable)
  {
    new Thread(runnable).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void nuclearOption(int delayMS)
  {
    runOutsideFXThread(delayMS, () -> Runtime.getRuntime().halt(0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void noOp()
  {
    assert Boolean.TRUE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runInFXThread(Runnable runnable)
  {
    if (Platform.isFxApplicationThread())
      runnable.run();
    else
      Platform.runLater(runnable);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void readResourceTextFile(String relPath, StringBuilder strBuilder, boolean keepEOLchars) throws IOException
  {
    assignSB(strBuilder, "");

    try (BufferedReader reader = new BufferedReader(new InputStreamReader(App.class.getResourceAsStream(relPath))))
    {
      String line;

      while ((line = reader.readLine()) != null)
      {
        if (keepEOLchars && (strBuilder.length() > 0))
          strBuilder.append("\n");

        strBuilder.append(line);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MediaType getMediaType(FilePath filePath)
  {
    if (filePath == null) return MediaType.OCTET_STREAM;

    Metadata metadata = new Metadata();
    metadata.set(Metadata.RESOURCE_NAME_KEY, filePath.toString());

    try (TikaInputStream stream = TikaInputStream.get(filePath.toPath()))
    {
      return tika.getDetector().detect(stream, metadata);
    }
    catch (IOException e)
    {
      return MediaType.OCTET_STREAM;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView getImageViewForRelativePath(String relPath)
  {
    return relPath.length() > 0 ? new ImageView(App.class.getResource(relPath).toString()) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ImageView getImageViewForRecordType(HDT_RecordType type)
  {
    return getImageViewForRelativePath(ui.getGraphicRelativePathByType(type));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getImageRelPathForFilePath(FilePath filePath, MediaType mimetype)
  {
    if (filePath.isDirectory())
      return getImageRelPathForFilePath(filePath, null, true);

    return getImageRelPathForFilePath(filePath, mimetype, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getImageRelPathForFilePath(FilePath filePath, MediaType mimetype, boolean isDir)
  {
    if (isDir)
      return "resources/images/folder.png";

    if (mimetype == null)
      mimetype = getMediaType(filePath);

    String imageName = "", typeStr = mimetype.toString();

    if (mimetype == MediaType.APPLICATION_XML)
      imageName = "document-code";
    else if (mimetype == MediaType.APPLICATION_ZIP)
      imageName = "vise-drawer";
    else if (typeStr.contains("pdf") || typeStr.contains("postscript") || typeStr.contains("framemaker"))
      imageName = "document-pdf";
    else if (typeStr.contains("djv") || typeStr.contains("book") || typeStr.contains("epub"))
      imageName = "book";
    else if (mimetype.getType().equals("image"))
      imageName = "image";
    else if (typeStr.contains("plain"))
      imageName = "document-text";
    else if (typeStr.contains("htm"))
      imageName = "text-html";
    else if (typeStr.contains("json"))
      imageName = "json";
    else if (typeStr.endsWith("tex"))
      imageName = "document-tex";
    else if (typeStr.contains("word") || typeStr.contains("rtf") || typeStr.contains("publisher") || typeStr.contains("mswrite") || typeStr.contains("writer") || typeStr.contains("msword") || typeStr.contains("xps"))
      imageName = "paper";
    else if (typeStr.contains("excel") || typeStr.contains("spread") || typeStr.contains("calc"))
      imageName = "table-sheet";
    else if (typeStr.contains("power") || typeStr.contains("presen") || typeStr.contains("impress"))
      imageName = "from_current_slide";
    else if (typeStr.contains("archi") || typeStr.contains("packa") || typeStr.contains("install") || typeStr.contains("diskimage"))
      imageName = "vise-drawer";
    else if (typeStr.contains("compress") || typeStr.contains("stuffit") || typeStr.contains("x-tar") || typeStr.contains("zip") || typeStr.contains("x-gtar") || typeStr.contains("lzma") || typeStr.contains("lzop") || typeStr.contains("x-xz"))
      imageName = "vise-drawer";
    else if (typeStr.contains("note"))
      imageName = "notebook-pencil";
    else if (typeStr.contains("chart") || typeStr.contains("ivio"))
      imageName = "chart";
    else if (typeStr.contains("kontour") || typeStr.contains("msmetafile") || typeStr.contains("emf") || typeStr.contains("wmf") || typeStr.contains("cgm") || typeStr.contains("dwg") || typeStr.contains("cmx") || typeStr.endsWith("eps") || typeStr.contains("freehand") || typeStr.contains("corel") || typeStr.contains("cdr") || typeStr.contains("draw") || typeStr.contains("karbon") || typeStr.contains("vector") || typeStr.contains("illustr"))
      imageName = "page_white_vector";
    else if (typeStr.contains("formul") || typeStr.contains("math"))
      imageName = "edit_mathematics";
    else if (typeStr.contains("graphic") || typeStr.contains("image"))
      imageName = "image";
    else if (mimetype.getType().equals("audio"))
      imageName = "sound_wave";
    else if (mimetype.getType().equals("video") || typeStr.contains("flash") || typeStr.contains("mp4"))
      imageName = "recording";
    else if (typeStr.contains("text") || typeStr.contains("docu"))
      imageName = "document-text";
    else
      imageName = "document";

    return "resources/images/" + imageName + ".png";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void ensureVisible(Stage stage, double defaultW, double defaultH)
  {
    if (Environment.isMac() == false) stage.setMaximized(false); // On Mac, this makes the window disappear

    stage.setFullScreen(false);
    stage.setIconified(false);

    stage.setX(Math.max(stage.getX(), 0.0));
    stage.setY(Math.max(stage.getY(), 0.0));

    if (stage.getWidth() < 250) stage.setWidth(defaultW);
    if (stage.getHeight() < 75) stage.setHeight(defaultH);

    double minX = Double.MAX_VALUE, minY = minX, maxX = Double.NEGATIVE_INFINITY, maxY = maxX;

    for (Screen screen : Screen.getScreens())
    {
      Rectangle2D bounds = screen.getBounds();

      minX = Math.min(minX, bounds.getMinX());
      minY = Math.min(minY, bounds.getMinY());
      maxX = Math.max(maxX, bounds.getMaxX());
      maxY = Math.max(maxY, bounds.getMaxY());
    }

    stage.setX(Math.min(stage.getX(), maxX - 50.0));
    stage.setY(Math.min(stage.getY(), maxY - 50.0));
    stage.setWidth(Math.min(stage.getWidth(), maxX - 100.0));
    stage.setHeight(Math.min(stage.getHeight(), maxY - 100.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void disableCache(Node node)
  {
    node.setCache(false);

    if (node instanceof Parent)
      Parent.class.cast(node).getChildrenUnmodifiable().forEach(Util::disableCache);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static JsonObj parseJsonObj(Reader in) throws IOException, org.json.simple.parser.ParseException
  { return new JsonObj((JSONObject) jsonParser.parse(in)); }

  public static JsonObj parseJsonObj(String str) throws org.json.simple.parser.ParseException
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

  public static <T>      void nullSwitch(T  obj,         Consumer<T>      ex) { if (obj != null)           ex.accept(obj); }
  public static <T>      T    nullSwitch(T  obj, T  def                     ) { return obj == null ? def : obj           ; }
  public static <T1, T2> T1   nullSwitch(T2 obj, T1 def, Function<T2, T1> ex) { return obj == null ? def : ex.apply(obj) ; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(Objects::nonNull).findFirst().orElse(null);
  }

  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func, Predicate<T1> pred)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(obj -> (obj != null) && pred.test(obj)).findFirst().orElse(null);
  }

  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, T1 def, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), def, func);
  }

  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), null, func);
  }

  public static <T> T findFirst(Iterable<T> iterable, Predicate<T> pred)
  {
    return StreamSupport.stream(iterable.spliterator(), false).filter(pred).findFirst().orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void deleteGridPaneRow(GridPane grid, final int rowNdx)
  {
    Set<Node> deleteNodes = new HashSet<>();

    grid.getChildren().forEach(child ->
    {
      int r = nullSwitch(GridPane.getRowIndex(child), 0);

      if (r > rowNdx)
        GridPane.setRowIndex(child, r - 1);
      else if (r == rowNdx)
        deleteNodes.add(child);
    });

    // remove nodes from row
    grid.getChildren().removeAll(deleteNodes);

    grid.getRowConstraints().remove(rowNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void deleteGridPaneColumn(GridPane grid, final int columnNdx)
  {
    Set<Node> deleteNodes = new HashSet<>();

    grid.getChildren().forEach(child ->
    {
      int c = nullSwitch(GridPane.getColumnIndex(child), 0);

      if (c > columnNdx)
        GridPane.setColumnIndex(child, c - 1);
      else if (c == columnNdx)
        deleteNodes.add(child);
    });

    // remove nodes from column
    grid.getChildren().removeAll(deleteNodes);

    grid.getColumnConstraints().remove(columnNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void sleepForMillis(long millis)
  {
    try { Thread.sleep(millis); }
    catch (InterruptedException e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> convertMultiLineStrToStrList(String str, boolean emptiesOK)
  {
    List<String> list = new ArrayList<>(Arrays.asList(str.split("\\r?\\n")));

    if (list.isEmpty()) return list;

    while (list.get(0).isBlank())
    {
      list.remove(0);
      if (list.isEmpty()) return list;
    }

    while (list.get(list.size() - 1).isBlank())
    {
      list.remove(list.size() - 1);
      if (list.isEmpty()) return list;
    }

    if (emptiesOK) return list;

    list.removeIf(s -> ultraTrim(s).isBlank());

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String randomHexStr(int size)          { return randomStr(size, "0123456789abcdef"); }
  public static String randomAlphanumericStr(int size) { return randomStr(size, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHILJLMNOPQRSTUVWXYZ"); }

  public static String randomStr(int size, String charsStr)
  {
    if (size < 0) return "";

    char[] chars = charsStr.toCharArray(), out = new char[size];
    double numChars = chars.length;

    for (int j = 0; j < size; j++)
    {
      double x = Math.random() * numChars;
      out[j] = chars[(int)x];
    }

    return new String(out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T> void addToSortedList(List<T> list, T item, Comparator<? super T> comp)
  {
    int ndx = binarySearch(list, item, comp);
    list.add(ndx >= 0 ? ndx + 1 : ~ndx, item);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T extends Comparable<? super T>> void addToSortedList(List<T> list, T item)
  {
    int ndx = binarySearch(list, item);
    list.add(ndx >= 0 ? ndx + 1 : ~ndx, item);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FilePath getHomeDir()
  {
    return new FilePath(FileSystemView.getFileSystemView().getHomeDirectory());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void enableAllIff (boolean enable,  EventTarget... targets) { disableAllIff(enable == false, targets); }
  public static void enableAll    (                 EventTarget... targets) { disableAllIff(false          , targets); }
  public static void disableAll   (                 EventTarget... targets) { disableAllIff(true           , targets); }

  public static void disableAllIff(boolean disable, EventTarget... targets)
  {
    List.of(targets).forEach(target ->
    {
      if      (target instanceof Node    ) Node    .class.cast(target).setDisable(disable);
      else if (target instanceof MenuItem) MenuItem.class.cast(target).setDisable(disable);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setAllVisible(boolean visible, EventTarget... targets)
  {
    List.of(targets).forEach(target ->
    {
      if      (target instanceof Node    ) Node    .class.cast(target).setVisible(visible);
      else if (target instanceof MenuItem) MenuItem.class.cast(target).setVisible(visible);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// DOI legal characters according to Crossref: "a-z", "A-Z", "0-9" and "-._;()/"
// But I've seen at least one Crossref DOI that included a colon

  public static String matchDOI(String str)
  {
    String doi = matchDOIiteration(str);

    return doi.length() > 0 ? doi : matchDOIiteration(unescapeURL(str));
  }

  private static String matchDOIiteration(String str)
  {
    str = prepareForIDMatch(str, false);

    Pattern p = Pattern.compile("(\\A|\\D)(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");
    Matcher m = p.matcher(safeStr(str));

    if (m.find()) return m.group(2);

    str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0').replace('°', '0');

    m = p.matcher(safeStr(str));

    if (m.find()) return m.group(2);

    p = Pattern.compile("([dD]0[i1])(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");
    m = p.matcher(safeStr(str));

    return m.find() ? m.group(2) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISSN(String str)
  {
    return matchISSN(str, null);
  }

  public static List<String> matchISSN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (safeStr(str).isEmpty()) return list;

    str = prepareForIDMatch(str, true);

    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])(\\d{4}-\\d{3}[\\dxX])(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);

    while (m.find())
    {
      String found = m.group(2).replace("-", "");
      int sum = 0;

      for (int x = 0; x < 8; x++)
      {
        char c = found.charAt(x);
        int n = c == 'X' ? 10 : parseInt(String.valueOf(c), -1);

        sum += n * (8 - x);
      }

      if ((sum > 0) && ((sum % 11) == 0))
      {
        found = m.group(2);

        if (list.contains(found) == false)
          list.add(found);
      }
    }

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISBN(String str)
  {
    return matchISBN(str, null);
  }

  public static List<String> matchISBN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (safeStr(str).isEmpty()) return list;

    matchISBNiteration(str, list);

    matchISBNiteration(str.replaceAll("\\h+", ""), list);  // remove horizontal whitespaces and check again

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String prepareForIDMatch(String str, boolean disregardLetters)
  {
    str = str.replaceAll("\\p{Pd}", "-")  // treat all dashes the same
             .replaceAll("\\u00AD", "-")  // "soft hyphen" is not included in the \p{Pd} class

        .replace('\u0002', '/'); // sometimes slash in DOI is encoded as STX control character

    if (disregardLetters)
      str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0').replace('°', '0');

    while (str.contains("--"))
      str = str.replace("--", "-");

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void matchISBNiteration(String str, List<String> list)
  {
    str = prepareForIDMatch(str, true);

    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){12}\\d)(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);

    while (m.find())
    {
      String found = m.group(2).replace("-", "");

      int n, sum = 0;
      for (int x = 0; x < 12; x++)
      {
        int coeff = ((x % 2) * 2) + 1;
        n = parseInt(String.valueOf(found.charAt(x)), -1);
        sum += coeff * n;
      }

      n = parseInt(StringUtils.right(found, 1), -1);

      if ((sum > 0) && (((10 - (sum % 10)) % 10) == n))
      {
        if (list.contains(found) == false)
          list.add(found);
      }
    }

    p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){9}[0-9xX])(\\z|[^0-9xX\\-])");
    m = p.matcher(str);

    while (m.find())
    {
      String found = m.group(2).toUpperCase().replace("-", "");
      int sum1 = 0, sum2 = 0;

      for (int x = 0; x < 10; x++)
      {
        char c = found.charAt(x);
        int n = c == 'X' ? 10 : parseInt(String.valueOf(c), -1);

        sum1 += n * (10 - x);
        sum2 += n * (x + 1);
      }

      if ((sum1 > 0) && (sum2 > 0) && ((sum1 % 11) == 0) && ((sum2 % 11) == 0) && (list.contains(found) == false))
        list.add(found);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Charset detectCharset(byte[] byteData)
  {
    CharsetDetector detector = new CharsetDetector();

    detector.setText(byteData);

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Charset detectCharset(InputStream streamData)
  {
    CharsetDetector detector = new CharsetDetector();

    try
    {
      detector.setText(streamData);
    }
    catch (IOException e)
    {
      return null;
    }

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setToolTip(Control ctrl, String str)
  {
    ctrl.setTooltip(safeStr(str).isBlank() ? null : new Tooltip(str));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
