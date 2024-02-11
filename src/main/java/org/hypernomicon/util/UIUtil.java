/*
 * Copyright 2015-2024 Jason Winning
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

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.lang.reflect.Constructor;
import java.util.*;

import javafx.beans.value.ObservableDoubleValue;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.view.WindowStack;

import com.google.common.collect.HashBasedTable;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Platform;
import javafx.beans.property.DoubleProperty;
import javafx.event.EventTarget;
import javafx.geometry.Orientation;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ComboBoxBase;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Control;
import javafx.scene.control.DialogPane;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.control.TreeTableRow;
import javafx.scene.control.TreeTableView;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.skin.ComboBoxListViewSkin;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.Window;

public final class UIUtil
{

//---------------------------------------------------------------------------

  private UIUtil() { throw new UnsupportedOperationException(); }

  private static final Map<String, Double> dividerMap = new HashMap<>();

  public static void setDividerPosition(SplitPane sp, String key, int ndx)
  {
    double pos = app.prefs.getDouble(key, -1.0);
    dividerMap.put(key, pos);

    if (pos >= 0)
      sp.setDividerPosition(ndx, pos);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void getDividerPosition(SplitPane sp, String key, int ndx)
  {
    double pos = sp.getDividerPositions()[ndx];
    if (Double.valueOf(pos).equals(dividerMap.get(key)) == false)
      app.prefs.putDouble(key, sp.getDividerPositions()[ndx]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MutableBoolean repositionPopupListWorkaround(MenuButton ctrl)
  {
    MutableBoolean adjusting = new MutableBoolean(false);

    ctrl.addEventFilter(MenuButton.ON_SHOWN, event ->    //////////////
    {                                                    //
      if (adjusting.isTrue()) return;                    //
                                                         // This is a workaround for the
      adjusting.setTrue();                               // fact that sometimes, when you show the
                                                         // popup list for a control, the popup list
      ctrl.hide();                                       // appears in the wrong place
      ctrl.show();                                       //
                                                         //
      adjusting.setFalse();                              //
    });                                                  //////////////

    return adjusting;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MutableBoolean repositionPopupListWorkaround(ComboBoxBase<?> ctrl)
  {
    MutableBoolean adjusting = new MutableBoolean(false);

    ctrl.addEventFilter(ComboBoxBase.ON_SHOWN, event ->  //////////////
    {                                                    //
      if (adjusting.isTrue()) return;                    //
                                                         // This is a workaround for the
      adjusting.setTrue();                               // fact that sometimes, when you show the
                                                         // popup list for a control, the popup list
      ctrl.hide();                                       // appears in the wrong place
      ctrl.show();                                       //
                                                         //
      adjusting.setFalse();                              //
    });                                                  //////////////

    return adjusting;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MenuItem copyOf(MenuItem orig)
  {
    MenuItem copy = new MenuItem(orig.getText());

    copy.setOnAction(orig.getOnAction());
    copy.setOnMenuValidation(orig.getOnMenuValidation());
    copy.setStyle(orig.getStyle());
    copy.setAccelerator(orig.getAccelerator());
    copy.setMnemonicParsing(orig.isMnemonicParsing());

    return copy;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setToolTip(Control ctrl, String str)
  {
    ctrl.setTooltip(makeTooltip(str));
  }

  public static void setToolTip(Tab ctrl, String str)
  {
    ctrl.setTooltip(makeTooltip(str));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Tooltip makeTooltip(String str)
  {
    if (safeStr(str).isBlank())
      return null;

    Tooltip tooltip = new Tooltip(str); // Font size is set in css file

    tooltip.setMaxWidth(1100);

    return tooltip;
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
      if      (target instanceof Node    ) ((Node    )target).setDisable(disable);
      else if (target instanceof Tab     ) ((Tab     )target).setDisable(disable);
      else if (target instanceof MenuItem) ((MenuItem)target).setDisable(disable);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setAllVisible(boolean visible, EventTarget... targets)
  {
    List.of(targets).forEach(target ->
    {
      if      (target instanceof Node    ) ((Node    )target).setVisible(visible);
      else if (target instanceof MenuItem) ((MenuItem)target).setVisible(visible);
    });
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
      ((Parent)node).getChildrenUnmodifiable().forEach(UIUtil::disableCache);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public
  static <T> ListView<T> getCBListView(ComboBox<T> cb)
  {
    return nullSwitch((ComboBoxListViewSkin<T>)cb.getSkin(), null, skin -> (ListView<T>) skin.getPopupContent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<Node> getChildren(Parent parent)
  {
    if (parent instanceof Pane   ) return ((Pane)parent).getChildren();
    if (parent instanceof ToolBar) return ((ToolBar)parent).getItems();

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The HTMLEditor is buggy if it is a child of multiple nodes

  public static Parent removeFromParent(Node node)
  {
    Parent parent = node.getParent();
    List<Node> children = getChildren(parent);

    if (children != null)
      children.remove(node);

    return parent;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addToParent(Node child, Parent parent)
  {
    List<Node> children = getChildren(parent);

    if ((children != null) && (children.contains(child) == false))
      children.add(child);
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
    if (rootChildren.isEmpty()) return;

    Node contextMenuContent = nullSwitch(rootChildren.get(0).lookup(".context-menu"), null,
                                         bridge -> ((Parent)bridge).getChildrenUnmodifiable().get(0));

    if (contextMenuContent == null) return;

    try
    {
      Class<?> contextMenuContentClass = Class.forName("com.sun.javafx.scene.control.ContextMenuContent"),
               menuItemContainerClass  = Class.forName("com.sun.javafx.scene.control.ContextMenuContent$MenuItemContainer");

      Constructor<?> ctor = menuItemContainerClass.getDeclaredConstructor(contextMenuContentClass, MenuItem.class);

      List<Node> list = ((VBox)contextMenuContentClass.getMethod("getItemsContainer").invoke(contextMenuContent)).getChildren();

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

  public static void setFontSize(Node node)
  {
    double fontSize = app.prefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize >= 1)
      node.setStyle("-fx-font-size: " + fontSize + "px;");
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

  public static void setHeights(Region region, Double ht)
  {
    region.setMinHeight (ht);
    region.setMaxHeight (ht);
    region.setPrefHeight(ht);
  }

  public static void setHeights(Stage stage, Double ht)
  {
    stage.setMinHeight(ht);
    stage.setMaxHeight(ht);
    stage.setHeight   (ht);
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

  public enum NodeUserDataType
  {
    Scaled,          // Data type: Boolean
    HypercCB         // Data type: HyperCB
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static void setNodeUserObj(Node node, NodeUserDataType dataType, Object newObj)
  {
    Object currentUserData = node.getUserData();

    if ((currentUserData != null) && (currentUserData instanceof Map) == false)
      return;

    Map<NodeUserDataType, Object> objMap = (Map<NodeUserDataType, Object>) currentUserData;

    if (objMap == null)
    {
      objMap = new EnumMap<>(NodeUserDataType.class);
      node.setUserData(objMap);
    }

    objMap.put(dataType, newObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static Object getNodeUserObj(Node node, NodeUserDataType dataType)
  {
    Object obj = node.getUserData();

    if ((obj instanceof Map) == false)
      return null;

    Map<NodeUserDataType, Object> objMap = (Map<NodeUserDataType, Object>)obj;
    return objMap.get(dataType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setIsScaled(Node node, boolean isScaled)
  {
    setNodeUserObj(node, NodeUserDataType.Scaled, isScaled);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean getIsScaled(Node node)
  {
    return nullSwitch((Boolean)getNodeUserObj(node, NodeUserDataType.Scaled), false, Boolean::booleanValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void scaleNodeForDPI(Node node)
  {
    if ((node == null) || getIsScaled(node)) return;

    setIsScaled(node, true);

    if (node.getStyleClass().contains("noScale"))
      return;

    boolean childrenOnly = node.getStyleClass().contains("childrenOnly");

    if (childrenOnly == false)
    {
      if (node instanceof Region)
      {
        Region region = (Region)node;

        scalePropertiesForDPI(region.prefHeightProperty(), region.prefWidthProperty(),
                              region.maxHeightProperty (), region.maxWidthProperty (),
                              region.minHeightProperty (), region.minWidthProperty ());
      }

      if (((node instanceof javafx.scene.shape.Path) == false) &&
          ((node instanceof javafx.scene.text.Text ) == false))
      {
        scalePropertiesForDPI(node.layoutXProperty(), node.layoutYProperty());
      }

      Double val = AnchorPane.getBottomAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setBottomAnchor(node, round(val * displayScale));

      val = AnchorPane.getTopAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setTopAnchor(node, round(val * displayScale));

      val = AnchorPane.getLeftAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setLeftAnchor(node, round(val * displayScale));

      val = AnchorPane.getRightAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setRightAnchor(node, round(val * displayScale));
    }

    if (node instanceof GridPane)
    {
      GridPane gridPane = (GridPane)node;

      gridPane.getColumnConstraints().forEach(cc -> scalePropertiesForDPI(cc.maxWidthProperty (), cc.minWidthProperty (), cc.prefWidthProperty ()));
      gridPane.getRowConstraints   ().forEach(rc -> scalePropertiesForDPI(rc.maxHeightProperty(), rc.minHeightProperty(), rc.prefHeightProperty()));
    }

    if ((node instanceof TreeTableView) || (node instanceof TableView))
    {
      (node instanceof TreeTableView ? ((TreeTableView<?>)node).getColumns() : ((TableView<?>)node).getColumns()).forEach(column ->
        scalePropertiesForDPI(column.maxWidthProperty(), column.minWidthProperty(), column.prefWidthProperty()));
    }
    else if (node instanceof MasterDetailPane)
    {
      MasterDetailPane mdp = (MasterDetailPane)node;

      scaleNodeForDPI(mdp.getMasterNode());
      scaleNodeForDPI(mdp.getDetailNode());
    }
    else if (node instanceof ToolBar)
      ((ToolBar)node).getItems().forEach(UIUtil::scaleNodeForDPI);
    else if (node instanceof TitledPane)
      scaleNodeForDPI(((TitledPane)node).getContent());
    else if (node instanceof TabPane)
      ((TabPane)node).getTabs().forEach(tab -> scaleNodeForDPI(tab.getContent()));
    else if (node instanceof Parent)
      ((Parent)node).getChildrenUnmodifiable().forEach(UIUtil::scaleNodeForDPI);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void scalePropertiesForDPI(DoubleProperty... props)
  {
    double[] vals = Arrays.stream(props).mapToDouble(ObservableDoubleValue::get).toArray();

    for (int ndx = 0; ndx < props.length; ndx++)
    {
      if (vals[ndx] > 0.0)
      {
        vals[ndx] = round(vals[ndx] * displayScale);
        props[ndx].set(vals[ndx]);
      }
    }
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

  public static DialogResult abortRetryIgnoreDialog(String msg)
  {
    return new PopupDialog(msg)

      .addButton("Abort" , mrAbort)
      .addButton("Retry" , mrRetry)
      .addButton("Ignore", mrIgnore)

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult yesNoCancelDialog(String msg)
  {
    return new PopupDialog(msg)

      .addButton("Yes"   , mrYes)
      .addButton("No"    , mrNo)
      .addButton("Cancel", mrCancel)

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult seriesConfirmDialog(String msg)
  {
    return new PopupDialog(msg)

      .addButton("Yes"       , mrYes)
      .addButton("No"        , mrNo)
      .addButton("Yes to all", mrYesToAll)
      .addButton("No to all" , mrNoToAll)

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean confirmDialog(String msg)
  {
    return new PopupDialog(msg)

      .addButton("Yes", mrYes)
      .addButton("No" , mrNo)

      .showModal() == mrYes;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean falseWithErrMsgCond(boolean showErrMsg, String errMsg)
  {
    return showErrMsg && falseWithErrorMessage(errMsg);
  }

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
    if (nodeToFocus != null) Platform.runLater(() -> safeFocus(nodeToFocus));
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum MessageDialogType { mtWarning, mtError, mtInformation }

  public static void messageDialog(String msg, MessageDialogType mt)
  {
    if (mt  == null) throw new NullPointerException("messageDialog type");
    if (msg == null) throw new NullPointerException("messageDialog msg" );

    runInFXThread(() ->
    {
      Alert alert;

      switch (mt)
      {
        case mtWarning :
          alert = new Alert(AlertType.WARNING);
          alert.setHeaderText("Warning");
          break;

        case mtInformation :
          alert = new Alert(AlertType.INFORMATION);
          alert.setHeaderText("Information");
          break;

        default :
          alert = new Alert(AlertType.ERROR);
          alert.setHeaderText("Error");
          break;
      }

      alert.setTitle(appTitle);
      alert.setContentText(msg);

      showAndWait(alert);

    }, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<Control, Double> rowHeight = new HashMap<>();
  private static final HashBasedTable<Control, Orientation, ScrollBar> sbMap = HashBasedTable.create();

  public static ScrollBar getScrollBar(Control ctrl, Orientation orientation)
  {
    ScrollBar sb = sbMap.get(ctrl, orientation);
    if (sb != null) return sb;

    for (Node node : ctrl.lookupAll(".scroll-bar"))
    {
      if (node instanceof ScrollBar)
      {
        sb = (ScrollBar) node;

        if (sb.getOrientation() == orientation)
        {
          sbMap.put(ctrl, orientation, sb);
          return sb;
        }
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static double getRowHeight(Control ctrl)
  {
    Double heightObj = rowHeight.get(ctrl);
    if (heightObj != null) return heightObj;

    for (Node rowNode : ctrl.lookupAll(".indexed-cell"))
    {
      if ((rowNode instanceof TableRow) || (rowNode instanceof TreeTableRow))
      {
        double height = ((Region) rowNode).getHeight();
        rowHeight.put(ctrl, height);
        return height;
      }
    }

    return 0.0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
