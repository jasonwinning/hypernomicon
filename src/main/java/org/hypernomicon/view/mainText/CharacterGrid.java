/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.view.mainText;

import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.OverrunStyle;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.*;

import java.util.Collection;
import java.util.Iterator;
import java.util.function.Consumer;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

class CharacterGrid
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final int colCount, rowCount;
  private final Consumer<Symbol> symbolHndlr;
  private final SymbolCtrl[][] symbolCtrls;
  private final Symbol[][] symbols;

  private int ndx = 0;
  private static SymbolCtrl focusCtrl = null;

//---------------------------------------------------------------------------

  public static class Symbol
  {
    public Symbol(int codepoint, String html, String desc)
    {
      this.codepoint = codepoint;
      this.html = safeStr(html).isBlank() ? "&#" + codepoint + ";" : html;
      this.desc = desc;
      ch = (char) codepoint;
    }

    private final Character ch;
    private final int codepoint;
    private final String html, desc;
    @Override public String toString() { return ch.toString(); }

    public int getCodePoint() { return codepoint; }
    public String getHTML()   { return html; }
    public String getDesc()   { return desc; }
  }

//---------------------------------------------------------------------------

  public class SymbolCtrl extends Hyperlink
  {
    final int col, row;

    public SymbolCtrl(String family, int col, int row)
    {
      super();

      this.col = col;
      this.row = row;
      setFont(Font.font(family, 24.0));
      setAlignment(Pos.CENTER);
      setCursor(Cursor.DEFAULT);
      setStyle("-fx-underline: false; -fx-text-fill: black;");
      setTextOverrun(OverrunStyle.CLIP);

      setAnchors(this, 2.0, 2.0, 2.0, 2.0);

      focusedProperty().addListener((obs, ov, nv) ->
      {
        if (Boolean.TRUE.equals(nv))
        {
          focusOnHyperlink(this);
          SymbolPickerDlgCtrlr.setBottomRowSelected(row == (rowCount - 1));
        }

        if ((obs == null) || (Boolean.TRUE.equals(nv) == false)) return;
        symbolHndlr.accept(symbols[col][row]);
      });

      setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
          SymbolPickerDlgCtrlr.insert();
      });

      symbolCtrls[col][row] = this;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CharacterGrid(GridPane gp, int colCount, int rowCount, Consumer<Symbol> symbolHndlr)
  {
    this.colCount = colCount;
    this.rowCount = rowCount;
    this.symbolHndlr = symbolHndlr;
    this.symbols = new Symbol[colCount][rowCount];

    symbolCtrls = new SymbolCtrl[colCount][rowCount];

    gp.getColumnConstraints().clear();
    AnchorPane parent = (AnchorPane) gp.getParent();

    for (int col = 0; col < colCount; col++)
    {
      ColumnConstraints cc = new ColumnConstraints(-1.0, -1.0, (parent.getPrefWidth() - 10.0) / colCount, Priority.SOMETIMES, null, true);
      gp.getColumnConstraints().add(cc);
    }

    gp.getRowConstraints().clear();

    for (int col = 0; col < rowCount; col++)
    {
      RowConstraints rc = new RowConstraints(-1.0, -1.0, gp.getPrefHeight() / rowCount, Priority.SOMETIMES, null, true);
      gp.getRowConstraints().add(rc);
    }

    for (int col = 0; col < colCount; col++)
      for (int row = 0; row < rowCount; row++)
      {
        SymbolCtrl symbolCtrl = new SymbolCtrl("Arial", col, row);

        AnchorPane ap = new AnchorPane(symbolCtrl);

        gp.add(ap, col, row);
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void focusOnHyperlink(SymbolCtrl symbolCtrl)
  {
    symbolCtrl.setBackground(new Background(new BackgroundFill(Color.YELLOW, null, null)));
    if ((focusCtrl != null) && (focusCtrl != symbolCtrl))
      focusCtrl.setBackground(null);

    focusCtrl = symbolCtrl;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setSymbols(Collection<Symbol> symbolCol)
  {
    Iterator<Symbol> it = symbolCol.iterator();

    while (it.hasNext() && (ndx < (rowCount * colCount)))
    {
      Symbol symbol = it.next();
      int row = ndx / colCount,
          col = ndx % colCount;

      assignCell(col, row, symbol, symbolCtrls[col][row]);

      ndx++;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignCell(int col, int row, Symbol symbol, Hyperlink hyperlink)
  {
    assignCell(col, row, symbol, hyperlink, true);
  }

  private void assignCell(int col, int row, Symbol symbol, Hyperlink hyperlink, boolean saveToPrefs)
  {
    symbols[col][row] = symbol;
    hyperlink.setText(symbol == null ? "" : symbol.toString());

    if ((saveToPrefs == false) || (row < (rowCount - 1))) return;

    if (symbol == null)
    {
      try
      {
        if (appPrefs.node("symbols").nodeExists(String.valueOf(col)))
          appPrefs.node("symbols").node(String.valueOf(col)).removeNode();
      }
      catch (BackingStoreException e)
      {
        messageDialog("An error occurred while accessing user-defined symbol preferences: " + e.getMessage(), mtError);
      }

      return;
    }

    Preferences node = appPrefs.node("symbols").node(String.valueOf(col));

    node.putInt("codePoint", symbol.codepoint);
    node.put("description", symbol.desc);
    node.put("html", symbol.html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void readPrefs() throws BackingStoreException
  {
    if (appPrefs.nodeExists("symbols") == false) return;
    Preferences symbolsNode = appPrefs.node("symbols");
    int row = rowCount - 1;

    for (int col = 0; col < colCount; col++)
    {
      if (symbolsNode.nodeExists(String.valueOf(col)))
      {
        Preferences node = symbolsNode.node(String.valueOf(col));
        int codePoint = node.getInt("codePoint", -1);

        if (codePoint > 0)
          assignCell(col, row, new Symbol(codePoint, node.get("html", ""), node.get("description", "")), symbolCtrls[col][row], false);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setSymbol(Symbol symbol)
  {
    assignCell(focusCtrl.col, focusCtrl.row, symbol, focusCtrl);

    if (symbol != null)
      symbolHndlr.accept(symbol);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Symbol getSymbol()
  {
    if (focusCtrl == null) return null;

    return symbols[focusCtrl.col][focusCtrl.row];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setFont(String newValue, boolean programmaticFontChange)
  {
    int focusCol = -1, focusRow = -1;

    if (focusCtrl != null)
    {
      focusCol = focusCtrl.col;
      focusRow = focusCtrl.row;
    }

    for (int col = 0; col < colCount; col++)
      for (int row = 0; row < rowCount; row++)
      {
        SymbolCtrl symbolCtrl = symbolCtrls[col][row];
        AnchorPane ap = (AnchorPane) symbolCtrl.getParent();
        String str = symbolCtrl.getText();

        removeFromParent(symbolCtrl);

        symbolCtrl = new SymbolCtrl(newValue, col, row);
        symbolCtrl.setText(str);
        addToParent(symbolCtrl, ap);
      }

    if (programmaticFontChange == false)
      appPrefs.node("symbols").put("font", newValue);

    if (focusCol > -1)
      focusOnHyperlink(symbolCtrls[focusCol][focusRow]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
