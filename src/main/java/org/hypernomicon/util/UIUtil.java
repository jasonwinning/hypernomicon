/*
 * Copyright 2015-2025 Jason Winning
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
import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.lang.reflect.Constructor;
import java.util.*;
import java.util.function.Supplier;

import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.controlsfx.control.MasterDetailPane;

import org.hypernomicon.dialogs.LockedDlgCtrlr;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.WindowStack;

import com.google.common.collect.HashBasedTable;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Platform;
import javafx.beans.property.DoubleProperty;
import javafx.event.EventTarget;
import javafx.geometry.Orientation;
import javafx.geometry.Rectangle2D;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.skin.ComboBoxListViewSkin;
import javafx.scene.layout.*;
import javafx.stage.*;

//---------------------------------------------------------------------------

public final class UIUtil
{

//---------------------------------------------------------------------------
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

  public static void setToolTip(Control ctrl, String str               ) { ctrl.setTooltip(makeTooltip(str     )); }
  public static void setToolTip(Tab     ctrl, String str               ) { ctrl.setTooltip(makeTooltip(str     )); }
  public static void setToolTip(Control ctrl, Supplier<String> supplier) { ctrl.setTooltip(makeTooltip(supplier)); }
  public static void setToolTip(Tab     ctrl, Supplier<String> supplier) { ctrl.setTooltip(makeTooltip(supplier)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Tooltip makeTooltip(Supplier<String> supplier)
  {
    Tooltip tooltip = new Tooltip();

    tooltip.setOnShowing(event -> tooltip.setText(safeStr(supplier.get())));
    tooltip.setMaxWidth(MAX_TOOLTIP_WIDTH);

    return tooltip;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Tooltip makeTooltip(String str)
  {
    if (strNullOrBlank(str))
      return null;

    Tooltip tooltip = new Tooltip(str); // Font size is set in css file

    tooltip.setMaxWidth(MAX_TOOLTIP_WIDTH);

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
      if      (target instanceof Node node        ) node    .setDisable(disable);
      else if (target instanceof Tab tab          ) tab     .setDisable(disable);
      else if (target instanceof MenuItem menuItem) menuItem.setDisable(disable);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setAllVisible(boolean visible, EventTarget... targets)
  {
    List.of(targets).forEach(target ->
    {
      if      (target instanceof Node node)         node    .setVisible(visible);
      else if (target instanceof MenuItem menuItem) menuItem.setVisible(visible);
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

  /**
   * Ensures that the specified {@code Stage} is visible on the screen with appropriate dimensions.
   *
   * <p>This method adjusts the position and size of the {@code Stage} to ensure it is fully visible
   * within the screen bounds. It sets the {@code Stage} to not be maximized, full screen, or iconified.
   * If the {@code Stage} is smaller than the specified minimum dimensions, it sets the {@code Stage} to
   * the default width and height. Additionally, it ensures that the {@code Stage} is within the bounds
   * of all available screens.
   *
   * <p>Note: On macOS, the method does not set the {@code Stage} to not be maximized as it may cause
   * the window to disappear.
   *
   * @param stage    the {@code Stage} to be adjusted. Cannot be null.
   * @param defaultW the default width of the {@code Stage} if its current width is less than 250.
   * @param defaultH the default height of the {@code Stage} if its current height is less than 75.
   * @throws NullPointerException if {@code stage} is null.
   */
  public static void ensureVisible(Stage stage, double defaultW, double defaultH)
  {
    Objects.requireNonNull(stage);

    if (Environment.isMac() == false) stage.setMaximized(false); // On Mac, this makes the window disappear

    stage.setFullScreen(false);
    stage.setIconified (false);

    if (stage.getWidth() < 250) stage.setWidth(defaultW);
    if (stage.getHeight() < 75) stage.setHeight(defaultH);

    double minX = Double.POSITIVE_INFINITY,
           minY = Double.POSITIVE_INFINITY,
           maxX = Double.NEGATIVE_INFINITY,
           maxY = Double.NEGATIVE_INFINITY;

    for (Screen screen : Screen.getScreens())
    {
      Rectangle2D bounds = screen.getBounds();

      minX = Math.min(minX, bounds.getMinX());
      minY = Math.min(minY, bounds.getMinY());
      maxX = Math.max(maxX, bounds.getMaxX());
      maxY = Math.max(maxY, bounds.getMaxY());
    }

    stage.setX     (Math.max(minX             , Math.min(stage.getX(), maxX - 50.0)));
    stage.setY     (Math.max(minY             , Math.min(stage.getY(), maxY - 50.0)));
    stage.setWidth (Math.min(stage.getWidth (), (maxX - minX) - 100.0));
    stage.setHeight(Math.min(stage.getHeight(), (maxY - minY) - 100.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void disableCache(Node node)
  {
    node.setCache(false);

    if (node instanceof Parent parent)
      parent.getChildrenUnmodifiable().forEach(UIUtil::disableCache);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <T> ListView<T> getCBListView(ComboBox<T> cb)
  {
    return nullSwitch((ComboBoxListViewSkin<T>) cb.getSkin(), null, skin -> (ListView<T>) skin.getPopupContent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<Node> getChildren(Parent parent)
  {
    if (parent instanceof Pane pane      ) return pane.getChildren();
    if (parent instanceof ToolBar toolBar) return toolBar.getItems();

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
    double fontSize = app.prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize >= 1)
      node.setStyle("-fx-font-size: " + fontSize + "px;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Copies the layout properties from one {@code Region} node (source) to another (target).
   *
   * <p>This method transfers the anchor pane constraints, grid pane indices and spans,
   * layout positions, and size constraints from {@code source} to {@code target}.
   *
   * @param source the source {@code Region} node whose layout properties are to be copied. Cannot be null.
   * @param target the target {@code Region} node to which the layout properties are copied. Cannot be null.
   * @throws NullPointerException if either {@code source} or {@code target} is null.
   */
  public static void copyRegionLayout(Region source, Region target)
  {
    setAnchors(target, AnchorPane.getTopAnchor (source), AnchorPane.getBottomAnchor(source),
                       AnchorPane.getLeftAnchor(source), AnchorPane.getRightAnchor (source));

    GridPane.setColumnIndex(target, GridPane.getColumnIndex(source));
    GridPane.setColumnSpan (target, GridPane.getColumnSpan (source));
    GridPane.setRowIndex   (target, GridPane.getRowIndex   (source));
    GridPane.setRowSpan    (target, GridPane.getRowSpan    (source));

    target.setLayoutX(source.getLayoutX());
    target.setLayoutY(source.getLayoutY());

    target.setMinSize (source.getMinWidth (), source.getMinHeight ());
    target.setMaxSize (source.getMaxWidth (), source.getMaxHeight ());
    target.setPrefSize(source.getPrefWidth(), source.getPrefHeight());
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

  public static void setHeights(Region region, double height)
  {
    region.setMinHeight (height);
    region.setMaxHeight (height);
    region.setPrefHeight(height);
  }

  public static void setHeights(Stage stage, double height)
  {
    stage.setMinHeight(height);
    stage.setMaxHeight(height);
    stage.setHeight   (height);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setWidths(Region region, double width)
  {
    region.setMinWidth (width);
    region.setMaxWidth (width);
    region.setPrefWidth(width);
  }

  public static void setWidths(Stage stage, double width)
  {
    stage.setMinWidth(width);
    stage.setMaxWidth(width);
    stage.setWidth   (width);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void safeFocus(Node node)
  {
    runInFXThread(() ->
    {
      if (node.isDisabled() == false)
        node.requestFocus();
    });
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
      if (node instanceof Region region)
      {
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
        AnchorPane.setBottomAnchor(node, scalePropertyValueForDPI(val));

      val = AnchorPane.getTopAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setTopAnchor(node, scalePropertyValueForDPI(val));

      val = AnchorPane.getLeftAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setLeftAnchor(node, scalePropertyValueForDPI(val));

      val = AnchorPane.getRightAnchor(node);
      if ((val != null) && (val > 0.0))
        AnchorPane.setRightAnchor(node, scalePropertyValueForDPI(val));
    }

    if (node instanceof GridPane gridPane)
    {
      gridPane.getColumnConstraints().forEach(cc -> scalePropertiesForDPI(cc.maxWidthProperty (), cc.minWidthProperty (), cc.prefWidthProperty ()));
      gridPane.getRowConstraints   ().forEach(rc -> scalePropertiesForDPI(rc.maxHeightProperty(), rc.minHeightProperty(), rc.prefHeightProperty()));
    }

    if ((node instanceof TreeTableView) || (node instanceof TableView))
    {
      (node instanceof TreeTableView ? ((TreeTableView<?>)node).getColumns() : ((TableView<?>)node).getColumns()).forEach(column ->
        scalePropertiesForDPI(column.maxWidthProperty(), column.minWidthProperty(), column.prefWidthProperty()));
    }
    else if (node instanceof MasterDetailPane mdp)
    {
      scaleNodeForDPI(mdp.getMasterNode());
      scaleNodeForDPI(mdp.getDetailNode());
    }
    else if (node instanceof ToolBar toolBar)
      toolBar.getItems().forEach(UIUtil::scaleNodeForDPI);
    else if (node instanceof TitledPane titledPane)
      scaleNodeForDPI(titledPane.getContent());
    else if (node instanceof TabPane tabPane)
      tabPane.getTabs().forEach(tab -> scaleNodeForDPI(tab.getContent()));
    else if (node instanceof SplitPane splitPane)
      splitPane.getItems().forEach(UIUtil::scaleNodeForDPI);
    else if (node instanceof Parent parent)
      parent.getChildrenUnmodifiable().forEach(UIUtil::scaleNodeForDPI);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static double scalePropertyValueForDPI(double val)
  {
    return Math.round(val * displayScale);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void scalePropertiesForDPI(DoubleProperty... props)
  {
    for (DoubleProperty prop : props)
    {
      double val = prop.get();

      if (val > 0.0)
        prop.set(scalePropertyValueForDPI(val));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void showLongMessage(String title, String text)
  {
    new LockedDlgCtrlr(title, text).showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void showStackTrace(Throwable e)
  {
    showLongMessage("Error", ExceptionUtils.getStackTrace(e));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FilePath   showDirDialog         (DirectoryChooser chooser) { return ui.windows.showDirDialog         (chooser); }
  public static FilePath   showOpenDialog        (FileChooser      chooser) { return ui.windows.showOpenDialog        (chooser); }
  public static FilePath   showSaveDialog        (FileChooser      chooser) { return ui.windows.showSaveDialog        (chooser); }
  public static List<File> showOpenMultipleDialog(FileChooser      chooser) { return ui.windows.showOpenMultipleDialog(chooser); }

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

      setHeights(dlgPane, 400);
      setWidths (dlgPane, 800);
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
    if (ui.dontInteract()) return mrIgnore;

    return new PopupDialog(msg)

      .addButton       ("Abort" , mrAbort )
      .addDefaultButton("Retry" , mrRetry )
      .addButton       ("Ignore", mrIgnore)

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult yesNoCancelDialog(String msg)
  {
    if (ui.dontInteract()) return mrCancel;

    return new PopupDialog(msg)

      .addButton       ("Yes"   , mrYes   )
      .addButton       ("No"    , mrNo    )
      .addDefaultButton("Cancel", mrCancel)

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static DialogResult seriesConfirmDialog(String msg)
  {
    if (ui.dontInteract()) return mrNoToAll;

    return new PopupDialog(msg)

      .addButton       ("Yes"       , mrYes     )
      .addDefaultButton("No"        , mrNo      )
      .addButton       ("Yes to all", mrYesToAll)
      .addButton       ("No to all" , mrNoToAll )

      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final String GO_BACK_BUTTON_CAPTION = "Go back";

  public static boolean confirmDialog(String msg, boolean yesIsDefault)
  {
    return confirmDialog(msg, "Yes", "No", yesIsDefault);
  }

  public static boolean confirmDialog(String msg, String yesCaption, String noCaption, boolean yesIsDefault)
  {
    if (ui.dontInteract()) return false;

    PopupDialog popupDialog = new PopupDialog(msg);

    if (yesIsDefault) popupDialog.addDefaultButton(yesCaption, mrYes).addButton       (noCaption , mrNo);
    else              popupDialog.addButton       (yesCaption, mrYes).addDefaultButton(noCaption , mrNo);

    return popupDialog.showModal() == mrYes;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Evaluates a condition and displays an error popup with the specified message if the condition is true.
   *
   * <p>This method checks if {@code showErrMsg} is true. If it is, it calls {@link #falseWithErrorPopup(String)}
   * with the provided error message {@code errMsg} and returns the result.
   *
   * @param showErrMsg a boolean indicating whether to show the error message.
   * @param errMsg     the error message to be displayed in the popup if {@code showErrMsg} is true. Cannot be null.
   * @return {@code false} if {@code showErrMsg} is true and an error popup is displayed; otherwise, the value of {@code showErrMsg}.
   * @throws NullPointerException if {@code errMsg} is null and {@code showErrMsg} is true.
   */
  public static boolean falseWithErrPopupCond(boolean showErrMsg, String errMsg)
  {
    return showErrMsg && falseWithErrorPopup(errMsg);
  }

  /**
   * Evaluates a condition and displays an error popup with a user-friendly message derived from the specified Throwable if the condition is true.
   *
   * <p>This method checks if {@code showErrMsg} is true. If it is, it calls {@link #falseWithErrorPopup(String)}
   * with a user-friendly message obtained from {@link #getThrowableMessage(Throwable)} using the provided Throwable {@code e}
   * and returns the result.
   *
   * @param showErrMsg a boolean indicating whether to show the error message.
   * @param e          the Throwable for which to get the user-friendly error message. Cannot be null.
   * @return {@code false} if {@code showErrMsg} is true and an error popup is displayed; otherwise, the value of {@code showErrMsg}.
   * @throws NullPointerException if {@code e} is null and {@code showErrMsg} is true.
   */
  public static boolean falseWithErrPopupCond(boolean showErrMsg, Throwable e)
  {
    return showErrMsg && falseWithErrorPopup(getThrowableMessage(e));
  }

  public static boolean falseWithInternalErrorPopup(int num) { return falseWithErrorPopup(new HDB_InternalError(num)); }
  public static boolean falseWithErrorPopup(Throwable e)     { return falseWithErrorPopup(getThrowableMessage(e)); }

  public static boolean falseWithErrorPopup  (String msg                  ) { return falseWithMessagePopup(msg, AlertType.ERROR      , null       ); }
  public static boolean falseWithErrorPopup  (String msg, Node nodeToFocus) { return falseWithMessagePopup(msg, AlertType.ERROR      , nodeToFocus); }
  public static boolean falseWithWarningPopup(String msg                  ) { return falseWithMessagePopup(msg, AlertType.WARNING    , null       ); }
  public static boolean falseWithWarningPopup(String msg, Node nodeToFocus) { return falseWithMessagePopup(msg, AlertType.WARNING    , nodeToFocus); }
  public static boolean falseWithInfoPopup   (String msg                  ) { return falseWithMessagePopup(msg, AlertType.INFORMATION, null       ); }
  public static boolean falseWithInfoPopup   (String msg, Node nodeToFocus) { return falseWithMessagePopup(msg, AlertType.INFORMATION, nodeToFocus); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean falseWithMessagePopup(String msg, AlertType type, Node nodeToFocus)
  {
    messagePopup(msg, type);
    if (nodeToFocus != null) Platform.runLater(() -> safeFocus(nodeToFocus));
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void errorPopup   (String msg)   { messagePopup(msg, AlertType.ERROR      ); }
  public static void infoPopup    (String msg)   { messagePopup(msg, AlertType.INFORMATION); }
  public static void warningPopup (String msg)   { messagePopup(msg, AlertType.WARNING    ); }

  public static void errorPopup(Throwable e)     { errorPopup(getThrowableMessage(e)); }
  public static void internalErrorPopup(int num) { errorPopup(new HDB_InternalError(num)); }

  /**
   * <p>Displays a popup message with the specified content and alert type.
   * This can be called from inside or outside of the JavaFX Application
   * thread. Either way, it will cause the calling thread to wait until
   * the user has clicked OK on the popup.
   *
   * <p>If the MainCtrlr object has been constructed, it will only show the
   * popup if ui.dontInteract() returns false. If ui.dontInteract() returns
   * true, the method will return immediately without displaying the popup.
   *
   * @param msg  the message to be displayed in the popup. Cannot be null.
   * @param type the type of the alert (e.g., WARNING, INFORMATION, ERROR). Cannot be null.
   * @throws NullPointerException if either {@code msg} or {@code type} is null.
   */
  private static void messagePopup(String msg, AlertType type)
  {
    Objects.requireNonNull(type, "Popup type");
    Objects.requireNonNull(msg , "Popup msg" );

    if ((ui != null) && ui.dontInteract()) return;

    runInFXThread(() ->
    {
      String theMsg = msg;
      AlertType theType = type;

      switch (type)
      {
        case WARNING : case INFORMATION : case ERROR :
          break;

        default :
          theType = AlertType.ERROR;
          theMsg = getThrowableMessage(new HDB_InternalError(10109));
          break;
      }

      Alert alert = new Alert(theType);
      alert.setHeaderText(titleCase(theType.toString()));
      alert.setTitle(appTitle);
      alert.setContentText(theMsg);

      showAndWait(alert);

    }, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<Control, Double> rowHeightMap = new HashMap<>();
  private static final HashBasedTable<Control, Orientation, ScrollBar> sbMap = HashBasedTable.create();

  private static ScrollBar getScrollBar(Control ctrl, Orientation orientation)
  {
    {
      ScrollBar sb = sbMap.get(ctrl, orientation);
      if (sb != null) return sb;
    }

    for (Node node : ctrl.lookupAll(".scroll-bar"))
    {
      if ((node instanceof ScrollBar sb) && (sb.getOrientation() == orientation))
      {
        sbMap.put(ctrl, orientation, sb);
        return sb;
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static double getRowHeight(Control ctrl)
  {
    Double heightObj = rowHeightMap.get(ctrl);
    if (heightObj != null) return heightObj;

    for (Node rowNode : ctrl.lookupAll(".indexed-cell"))
    {
      if ((rowNode instanceof TableRow) || (rowNode instanceof TreeTableRow))
      {
        double height = ((Region) rowNode).getHeight();
        rowHeightMap.put(ctrl, height);
        return height;
      }
    }

    return 0.0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scrolls a row in a TableView or TreeTableView to make it visible within the viewport.
   *
   * @param tableCtrl The control to scroll, either a {@link TableView} or {@link TreeTableView}.
   *                  Must be an instance of one of these types.
   * @param ndx       The index of the row to scroll to.
   *                  If the row is already within the viewport, no scrolling is performed.
   * @throws IllegalArgumentException If {@code tableCtrl} is neither a {@link TableView}
   *                                  nor a {@link TreeTableView}.
   */
  public static void scrollToNdxInTable(Control tableCtrl, int ndx)
  {
    // The way this works is better than TableView.scrollTo
    // scrollTo changes the scroll position even if the row in question was already in view

    // TreeTableView.scrollTo could not be used because it is too buggy.
    // In java.scene.control.skin.VirtualFlow.adjustPositionToIndex, variable "estimatedSize" is often incorrectly set to 1,
    // which causes it to just scroll to the top regardless of the index passed in.

    ScrollBar verticalScrollBar = getScrollBar(tableCtrl, Orientation.VERTICAL);
    if (verticalScrollBar == null) return;

    double allRowsHeight, rowHeight = getRowHeight(tableCtrl);

    if      (tableCtrl instanceof TableView<?>     tableView)     allRowsHeight = rowHeight * tableView.getItems().size();
    else if (tableCtrl instanceof TreeTableView<?> treeTableView) allRowsHeight = rowHeight * treeTableView.getExpandedItemCount();
    else
      throw new IllegalArgumentException("tableCtrl must be a TableView or TreeTableView");

    double viewportHeight   = allRowsHeight * verticalScrollBar.getVisibleAmount(),
           scrollableHeight = allRowsHeight - viewportHeight,
           viewportStartY   = scrollableHeight * verticalScrollBar.getValue(),
           viewportEndY     = viewportStartY + viewportHeight,

           rowStartY = ndx * rowHeight,
           rowEndY   = rowStartY + rowHeight,

           newScrollPosition;

    if      (rowStartY < viewportStartY) newScrollPosition = ((rowStartY - (viewportHeight / 2)) + (rowHeight / 2)) / scrollableHeight;
    else if (rowEndY   > viewportEndY  ) newScrollPosition = ((rowEndY   - (viewportHeight / 2)) - (rowHeight / 2)) / scrollableHeight;
    else                                 return;  // The row is already within the viewport

    verticalScrollBar.setValue(Math.max(0, Math.min(newScrollPosition, 1)));  // Ensure within bounds
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void forceToggleSelection(ToggleGroup tg)
  {
    tg.selectedToggleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
        oldValue.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
