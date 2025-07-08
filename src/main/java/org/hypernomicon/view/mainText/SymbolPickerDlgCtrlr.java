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

package org.hypernomicon.view.mainText;

import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.view.mainText.CharacterGrid.Symbol;
import org.jsoup.parser.Parser;

import com.google.common.collect.ImmutableList;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.prefs.BackingStoreException;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;
import javafx.scene.web.WebEngine;

//---------------------------------------------------------------------------

public final class SymbolPickerDlgCtrlr extends NonmodalWindow
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnInsert, btnCancel;
  @FXML private CheckBox chkUseFont;
  @FXML private ComboBox<String> cbFont;
  @FXML private GridPane gp;
  @FXML private TextField tfChar, tfCodePoint, tfHex, tfHTML, tfDesc;

  private static SymbolPickerDlgCtrlr instance = null;

  private static final int ROW_COUNT = 8, COL_COUNT = 32;

  private final CharacterGrid charGrid;
  private final ImmutableList<Symbol> chars8851, symbols8851, math, greek, misc;

  private boolean programmaticFontChange = false, programmaticChange = false;

//---------------------------------------------------------------------------

  public static void close(boolean exitingApp) { close(instance, exitingApp); }

  @Override protected void getDividerPositions() { }
  @Override protected void setDividerPositions() { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SymbolPickerDlgCtrlr()
  {
    super("view/mainText/SymbolPickerDlg", "Symbol");

    bottomRowSelected(false);

    charGrid = new CharacterGrid(gp, COL_COUNT, ROW_COUNT, symbol ->
    {
      programmaticChange = true;

      if (symbol == null)
      {
        tfChar.setText("");
        tfCodePoint.setText("");
        tfHex.setText("");
        tfHTML.setText("");
        tfDesc.setText("");

        programmaticChange = false;
        return;
      }

      tfChar.setText(symbol.toString());
      tfCodePoint.setText(String.valueOf(symbol.getCodePoint()));
      tfHex.setText(String.format("%04X", symbol.getCodePoint()));

      tfHTML.setText(symbol.getHTML());
      tfDesc.setText(symbol.getDesc());

      programmaticChange = false;
    });

    tfChar.textProperty().addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;

      nv = stripSafe(nv);
      if (nv.length() != 1)
      {
        programmaticChange = true;

        tfCodePoint.setText("");
        tfHex.setText("");
        tfHTML.setText("");
        charGrid.setSymbol(null);

        programmaticChange = false;
        return;
      }

      charGrid.setSymbol(new Symbol(nv.codePointAt(0), "", ""));
    });

    tfCodePoint.textProperty().addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;

      nv = stripSafe(nv);
      int codePoint = parseInt(nv, -1);

      if ((codePoint < 1) || (codePoint > 65535))
      {
        programmaticChange = true;

        tfChar.setText("");
        tfHex.setText("");
        tfHTML.setText("");
        charGrid.setSymbol(null);

        programmaticChange = false;
        return;
      }

      charGrid.setSymbol(new Symbol(codePoint, "", ""));
    });

    tfHex.textProperty().addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;

      nv = stripSafe(nv);
      int codePoint = parseHex(nv, -1);

      if ((codePoint < 1) || (codePoint > 65535))
      {
        programmaticChange = true;

        tfChar.setText("");
        tfCodePoint.setText("");
        tfHTML.setText("");
        charGrid.setSymbol(null);

        programmaticChange = false;
        return;
      }

      charGrid.setSymbol(new Symbol(codePoint, "", ""));
    });

    tfHTML.textProperty().addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;

      nv = stripSafe(nv);
      String character = Parser.unescapeEntities(nv, false);

      if ((nv.endsWith(";") == false) || (character.length() != 1) || (character.equals(nv)))
      {
        programmaticChange = true;

        tfChar.setText("");
        tfCodePoint.setText("");
        tfHex.setText("");
        charGrid.setSymbol(null);

        programmaticChange = false;
        return;
      }

      charGrid.setSymbol(new Symbol(character.codePointAt(0), nv, ""));
    });

    tfDesc.textProperty().addListener((obs, ov, nv) ->
    {
      if (programmaticChange) return;

      Symbol oldSymbol = charGrid.getSymbol();
      if (oldSymbol == null) return;

      charGrid.setSymbol(new Symbol(oldSymbol.getCodePoint(), oldSymbol.getHTML(), safeStr(nv)));
    });

    chars8851   = initChars8851  ();
    symbols8851 = initSymbols8851();
    math        = initMath       ();
    greek       = initGreek      ();
    misc        = initMisc       ();

    setToolTip(cbFont, "Set the font that characters will be displayed in");

    cbFont.setCellFactory(param -> new ListCell<>()
    {
      @Override public void updateItem(String item, boolean empty)
      {
        super.updateItem(item, empty);
        if (item != null)
        {
          setText(item);
          setFont(Font.font(item, 12.0));
        }
      }
    });

    Platform.runLater(() ->
    {
      final ObservableList<String> fonts = FXCollections.observableArrayList(Font.getFamilies());
      fonts.add(0, "");
      programmaticFontChange = true;

      cbFont.setValue("");
      cbFont.setItems(fonts);

      programmaticFontChange = false;
    });

    Platform.runLater(() ->
    {
      String font = app.prefs.node("symbols").get("font", "Arial");
      if (strNullOrBlank(font)) font = "Arial";

      programmaticFontChange = true;
      cbFont.getSelectionModel().select(font);
      programmaticFontChange = false;

      charGrid.setSymbols(math);
      charGrid.setSymbols(greek);
      charGrid.setSymbols(misc);
      charGrid.setSymbols(chars8851);
      charGrid.setSymbols(symbols8851);

      try
      {
        charGrid.readPrefs();
      }
      catch (BackingStoreException e)
      {
        errorPopup("An error occurred while reading user-defined symbols: " + getThrowableMessage(e));
      }
    });

    chkUseFont.setSelected(app.prefs.node("symbols").getBoolean("useFont", false));
    chkUseFont.selectedProperty().addListener((obs, oldValue, newValue) ->
    {
      if      (Boolean.TRUE .equals(newValue)) app.prefs.node("symbols").putBoolean("useFont", true);
      else if (Boolean.FALSE.equals(newValue)) app.prefs.node("symbols").putBoolean("useFont", false);
    });

    cbFont.getSelectionModel().selectedItemProperty().addListener((obs, oldValue, newValue) -> charGrid.setFont(newValue, programmaticFontChange));

    btnInsert.setOnAction(event -> insertClick());

    btnCancel.setOnAction(event -> close(false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show()
  {
    if (instance == null)
      instance = new SymbolPickerDlgCtrlr();

    show(instance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void setBottomRowSelected(boolean bottomSelected)
  {
    if (instance != null) instance.bottomRowSelected(bottomSelected);
  }

  private void bottomRowSelected(boolean bottomSelected)
  {
    tfChar     .setEditable(bottomSelected);
    tfCodePoint.setEditable(bottomSelected);
    tfDesc     .setEditable(bottomSelected);
    tfHex      .setEditable(bottomSelected);
    tfHTML     .setEditable(bottomSelected);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void insert()
  {
    if (instance != null) instance.insertClick();
  }

  private void insertClick()
  {
    if ((db.isLoaded() == false) || (ui.activeTab() == null)) return;

    MainTextWrapper mtWrapper = ui.activeTab().mainTextWrapper();
    if ((mtWrapper == null) || (mtWrapper.isEditing() == false)) return;

    WebEngine engine = MainTextWrapper.getEditorEngine();
    if (engine == null) return;

    Symbol symbol = charGrid.getSymbol();
    if (symbol == null) return;

    String text;

    if (chkUseFont.isSelected())
    {
      String font = cbFont.getValue();
      if (font.isBlank())
        text = htmlEscaper.escape(tfChar.getText());
      else
        text = "<span style=\"font-family: &quot;" + font + "&quot;;\">" + htmlEscaper.escape(tfChar.getText()) + "</span>";
    }
    else
      text = tfHTML.getText();

    engine.executeScript("insertHtmlAtCursor('" + text + "')");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableList<Symbol> initChars8851()
  {
    return ImmutableList.<Symbol>builder()

      .add(new Symbol(192, "&Agrave;", "Capital a with grave accent"))   // À
      .add(new Symbol(193, "&Aacute;", "Capital a with acute accent"))   // Á
      .add(new Symbol(194, "&Acirc;", "Capital a with circumflex accent"))   // Â
      .add(new Symbol(195, "&Atilde;", "Capital a with tilde"))   // Ã
      .add(new Symbol(196, "&Auml;", "Capital a with umlaut"))   // Ä
      .add(new Symbol(197, "&Aring;", "Capital a with ring"))   // Å
      .add(new Symbol(198, "&AElig;", "Capital ae"))   // Æ
      .add(new Symbol(199, "&Ccedil;", "Capital c with cedilla"))   // Ç
      .add(new Symbol(200, "&Egrave;", "Capital e with grave accent"))   // È
      .add(new Symbol(201, "&Eacute;", "Capital e with acute accent"))   // É
      .add(new Symbol(202, "&Ecirc;", "Capital e with circumflex accent"))   // Ê
      .add(new Symbol(203, "&Euml;", "Capital e with umlaut"))   // Ë
      .add(new Symbol(204, "&Igrave;", "Capital i with grave accent"))   // Ì
      .add(new Symbol(205, "&Iacute;", "Capital i with accute accent"))   // Í
      .add(new Symbol(206, "&Icirc;", "Capital i with circumflex accent"))   // Î
      .add(new Symbol(207, "&Iuml;", "Capital i with umlaut"))   // Ï
      .add(new Symbol(208, "&ETH;", "Capital eth (Icelandic)"))   // Ð
      .add(new Symbol(209, "&Ntilde;", "Capital n with tilde"))   // Ñ
      .add(new Symbol(210, "&Ograve;", "Capital o with grave accent"))   // Ò
      .add(new Symbol(211, "&Oacute;", "Capital o with accute accent"))   // Ó
      .add(new Symbol(212, "&Ocirc;", "Capital o with circumflex accent"))   // Ô
      .add(new Symbol(213, "&Otilde;", "Capital o with tilde"))   // Õ
      .add(new Symbol(214, "&Ouml;", "Capital o with umlaut"))   // Ö
      .add(new Symbol(216, "&Oslash;", "Capital o with slash"))   // Ø
      .add(new Symbol(217, "&Ugrave;", "Capital u with grave accent"))   // Ù
      .add(new Symbol(218, "&Uacute;", "Capital u with acute accent"))   // Ú
      .add(new Symbol(219, "&Ucirc;", "Capital u with circumflex accent"))   // Û
      .add(new Symbol(220, "&Uuml;", "Capital u with umlaut"))   // Ü
      .add(new Symbol(221, "&Yacute;", "Capital y with acute accent"))   // Ý
      .add(new Symbol(222, "&THORN;", "Capital thorn (Icelandic)"))   // Þ
      .add(new Symbol(223, "&szlig;", "Lowercase sharp s (German)"))   // ß
      .add(new Symbol(224, "&agrave;", "Lowercase a with grave accent"))   // à
      .add(new Symbol(225, "&aacute;", "Lowercase a with acute accent"))   // á
      .add(new Symbol(226, "&acirc;", "Lowercase a with circumflex accent"))   // â
      .add(new Symbol(227, "&atilde;", "Lowercase a with tilde"))   // ã
      .add(new Symbol(228, "&auml;", "Lowercase a with umlaut"))   // ä
      .add(new Symbol(229, "&aring;", "Lowercase a with ring"))   // å
      .add(new Symbol(230, "&aelig;", "Lowercase ae"))   // æ
      .add(new Symbol(231, "&ccedil;", "Lowercase c with cedilla"))   // ç
      .add(new Symbol(232, "&egrave;", "Lowercase e with grave accent"))   // è
      .add(new Symbol(233, "&eacute;", "Lowercase e with acute accent"))   // é
      .add(new Symbol(234, "&ecirc;", "Lowercase e with circumflex accent"))   // ê
      .add(new Symbol(235, "&euml;", "Lowercase e with umlaut"))   // ë
      .add(new Symbol(236, "&igrave;", "Lowercase i with grave accent"))   // ì
      .add(new Symbol(237, "&iacute;", "Lowercase i with acute accent"))   // í
      .add(new Symbol(238, "&icirc;", "Lowercase i with circumflex accent"))   // î
      .add(new Symbol(239, "&iuml;", "Lowercase i with umlaut"))   // ï
      .add(new Symbol(240, "&eth;", "Lowercase eth (Icelandic)"))   // ð
      .add(new Symbol(241, "&ntilde;", "Lowercase n with tilde"))   // ñ
      .add(new Symbol(242, "&ograve;", "Lowercase o with grave accent"))   // ò
      .add(new Symbol(243, "&oacute;", "Lowercase o with acute accent"))   // ó
      .add(new Symbol(244, "&ocirc;", "Lowercase o with circumflex accent"))   // ô
      .add(new Symbol(245, "&otilde;", "Lowercase o with tilde"))   // õ
      .add(new Symbol(246, "&ouml;", "Lowercase o with umlaut"))   // ö
      .add(new Symbol(248, "&oslash;", "Lowercase o with slash"))   // ø
      .add(new Symbol(249, "&ugrave;", "Lowercase u with grave accent"))   // ù
      .add(new Symbol(250, "&uacute;", "Lowercase u with acute accent"))   // ú
      .add(new Symbol(251, "&ucirc;", "Lowercase u with circumflex accent"))   // û
      .add(new Symbol(252, "&uuml;", "Lowercase u with umlaut"))   // ü
      .add(new Symbol(253, "&yacute;", "Lowercase y with acute accent"))   // ý
      .add(new Symbol(254, "&thorn;", "Lowercase thorn (Icelandic)"))   // þ
      .add(new Symbol(255, "&yuml;", "Lowercase y with umlaut"))   // ÿ
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableList<Symbol> initSymbols8851()
  {
    return ImmutableList.<Symbol>builder()

      .add(new Symbol(161, "&iexcl;", "Inverted exclamation mark"))   // ¡
      .add(new Symbol(162, "&cent;", "Cent"))   // ¢
      .add(new Symbol(163, "&pound;", "Pound"))   // £
      .add(new Symbol(164, "&curren;", "Currency"))   // ¤
      .add(new Symbol(165, "&yen;", "Yen"))   // ¥
      .add(new Symbol(166, "&brvbar;", "Broken vertical bar"))   // ¦
      .add(new Symbol(167, "&sect;", "Section"))   // §
      .add(new Symbol(171, "&laquo;", "Opening/Left angle quotation mark"))   // «
      .add(new Symbol(173, "&shy;", "Soft hyphen"))   // ­
      .add(new Symbol(176, "&deg;", "Degree"))   // °
      .add(new Symbol(177, "&plusmn;", "Plus or minus"))   // ±
      .add(new Symbol(181, "&micro;", "Micro"))   // µ
      .add(new Symbol(182, "&para;", "Paragraph"))   // ¶
      .add(new Symbol(187, "&raquo;", "Closing/Right angle quotation mark"))   // »
      .add(new Symbol(188, "&frac14;", "Fraction 1/4"))   // ¼
      .add(new Symbol(189, "&frac12;", "Fraction 1/2"))   // ½
      .add(new Symbol(190, "&frac34;", "Fraction 3/4"))   // ¾
      .add(new Symbol(191, "&iquest;", "Inverted question mark"))   // ¿
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableList<Symbol> initMath()
  {
    return ImmutableList.<Symbol>builder()

      .add(new Symbol(215, "&times;", "Multiplication"))   // ×
      .add(new Symbol(247, "&divide;", "Divide"))   // ÷
      .add(new Symbol(8242, "&prime;", "Minutes (Degrees)"))   // ′
      .add(new Symbol(8243, "&Prime;", "Seconds (Degrees)"))   // ″
      .add(new Symbol(9671, "&#9671;", "It is possible that"))  // ◇
      .add(new Symbol(9723, "&#9723;", "It is necessary that")) // ◻
      .add(new Symbol(8592, "&larr;", "Left arrow"))   // ←
      .add(new Symbol(8593, "&uarr;", "Up arrow"))   // ↑
      .add(new Symbol(8658, "&rArr;", "Implies"))  // ⇒
      .add(new Symbol(8866, "&#8866;", "Proves; turnstile"))  // ⊢
      .add(new Symbol(8876, "&#8876;", "Does not prove"))  // ⊬
      .add(new Symbol(8872, "&#8872;", "Models; double turnstile"))  // ⊨
      .add(new Symbol(8877, "&#8877;", "Does not model"))  // ⊭
      .add(new Symbol(8594, "&rarr;", "Right arrow"))   // →
      .add(new Symbol(8595, "&darr;", "Down arrow"))   // ↓
      .add(new Symbol(8596, "&harr;", "Left right arrow"))   // ↔
      .add(new Symbol(8660, "&hArr;", "If and only if"))  // ⇔
      .add(new Symbol(172, "&not;", "Negation"))   // ¬
      .add(new Symbol(8704, "&forall;", "For all"))   // ∀
      .add(new Symbol(8706, "&part;", "Part"))   // ∂
      .add(new Symbol(8707, "&exist;", "Exist"))   // ∃
      .add(new Symbol(8709, "&empty;", "Empty"))   // ∅
      .add(new Symbol(8711, "&nabla;", "Nabla"))   // ∇
      .add(new Symbol(8712, "&isin;", "Is in"))   // ∈
      .add(new Symbol(8713, "&notin;", "Not in"))   // ∉
      .add(new Symbol(8715, "&ni;", "Ni"))   // ∋
      .add(new Symbol(8719, "&prod;", "Product"))   // ∏
      .add(new Symbol(8721, "&sum;", "Sum"))   // ∑
      .add(new Symbol(8722, "&minus;", "Minus"))   // −
      .add(new Symbol(8727, "&lowast;", "Asterisk (Lowast)"))   // ∗
      .add(new Symbol(8730, "&radic;", "Square root"))   // √
      .add(new Symbol(8733, "&prop;", "Proportional to"))   // ∝
      .add(new Symbol(8734, "&infin;", "Infinity"))   // ∞
      .add(new Symbol(8736, "&ang;", "Angle"))   // ∠
      .add(new Symbol(8743, "&and;", "And"))   // ∧
      .add(new Symbol(183,  "&middot;", "Conjunction"))  // ·
      .add(new Symbol(8744, "&or;", "Or"))   // ∨
      .add(new Symbol(8741, "&#8741;", "Parallel"))  // ∥
      .add(new Symbol(8891, "&#8891;", "Exclusive Or"))  // ⊻
      .add(new Symbol(8745, "&cap;", "Cap"))   // ∩
      .add(new Symbol(8746, "&cup;", "Cup"))   // ∪
      .add(new Symbol(8747, "&int;", "Integral"))   // ∫
      .add(new Symbol(8756, "&there4;", "Therefore"))   // ∴
      .add(new Symbol(8764, "&sim;", "Similar to"))   // ∼
      .add(new Symbol(8773, "&cong;", "Congurent to"))   // ≅
      .add(new Symbol(8776, "&asymp;", "Almost equal"))   // ≈
      .add(new Symbol(8800, "&ne;", "Not equal"))   // ≠
      .add(new Symbol(8801, "&equiv;", "Equivalent"))   // ≡
      .add(new Symbol(8802, "&#8802;", "Not equivalent"))  // ≢
      .add(new Symbol(8788, "&#8788;", "Is defined as"))  // ≔
      .add(new Symbol(8797, "&#8797;", "Equal to by definition"))  // ≝
      .add(new Symbol(8804, "&le;", "Less or equal"))   // ≤
      .add(new Symbol(8805, "&ge;", "Greater or equal"))   // ≥
      .add(new Symbol(8834, "&sub;", "Subset of"))   // ⊂
      .add(new Symbol(8835, "&sup;", "Superset of"))   // ⊃
      .add(new Symbol(8836, "&nsub;", "Not subset of"))   // ⊄
      .add(new Symbol(8838, "&sube;", "Subset or equal"))   // ⊆
      .add(new Symbol(8839, "&supe;", "Superset or equal"))   // ⊇
      .add(new Symbol(8853, "&oplus;", "Circled plus"))   // ⊕
      .add(new Symbol(8855, "&otimes;", "Circled times"))   // ⊗
      .add(new Symbol(8869, "&perp;", "Perpendicular"))   // ⊥
      .add(new Symbol(8901, "&sdot;", "Dot operator"))   // ⋅
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableList<Symbol> initGreek()
  {
    return ImmutableList.<Symbol>builder()

      .add(new Symbol(913, "&Alpha;", "Alpha"))   // Α
      .add(new Symbol(914, "&Beta;", "Beta"))   // Β
      .add(new Symbol(915, "&Gamma;", "Gamma"))   // Γ
      .add(new Symbol(916, "&Delta;", "Delta"))   // Δ
      .add(new Symbol(917, "&Epsilon;", "Epsilon"))   // Ε
      .add(new Symbol(918, "&Zeta;", "Zeta"))   // Ζ
      .add(new Symbol(919, "&Eta;", "Eta"))   // Η
      .add(new Symbol(920, "&Theta;", "Theta"))   // Θ
      .add(new Symbol(921, "&Iota;", "Iota"))   // Ι
      .add(new Symbol(922, "&Kappa;", "Kappa"))   // Κ
      .add(new Symbol(923, "&Lambda;", "Lambda"))   // Λ
      .add(new Symbol(924, "&Mu;", "Mu"))   // Μ
      .add(new Symbol(925, "&Nu;", "Nu"))   // Ν
      .add(new Symbol(926, "&Xi;", "Xi"))   // Ξ
      .add(new Symbol(927, "&Omicron;", "Omicron"))   // Ο
      .add(new Symbol(928, "&Pi;", "Pi"))   // Π
      .add(new Symbol(929, "&Rho;", "Rho"))   // Ρ
      .add(new Symbol(931, "&Sigma;", "Sigma"))   // Σ
      .add(new Symbol(932, "&Tau;", "Tau"))   // Τ
      .add(new Symbol(933, "&Upsilon;", "Upsilon"))   // Υ
      .add(new Symbol(934, "&Phi;", "Phi"))   // Φ
      .add(new Symbol(935, "&Chi;", "Chi"))   // Χ
      .add(new Symbol(936, "&Psi;", "Psi"))   // Ψ
      .add(new Symbol(937, "&Omega;", "Omega"))   // Ω
      .add(new Symbol(945, "&alpha;", "alpha"))   // α
      .add(new Symbol(946, "&beta;", "beta"))   // β
      .add(new Symbol(947, "&gamma;", "gamma"))   // γ
      .add(new Symbol(948, "&delta;", "delta"))   // δ
      .add(new Symbol(949, "&epsilon;", "epsilon"))   // ε
      .add(new Symbol(950, "&zeta;", "zeta"))   // ζ
      .add(new Symbol(951, "&eta;", "eta"))   // η
      .add(new Symbol(952, "&theta;", "theta"))   // θ
      .add(new Symbol(953, "&iota;", "iota"))   // ι
      .add(new Symbol(954, "&kappa;", "kappa"))   // κ
      .add(new Symbol(955, "&lambda;", "lambda"))   // λ
      .add(new Symbol(956, "&mu;", "mu"))   // μ
      .add(new Symbol(957, "&nu;", "nu"))   // ν
      .add(new Symbol(958, "&xi;", "xi"))   // ξ
      .add(new Symbol(959, "&omicron;", "omicron"))   // ο
      .add(new Symbol(960, "&pi;", "pi"))   // π
      .add(new Symbol(961, "&rho;", "rho"))   // ρ
      .add(new Symbol(962, "&sigmaf;", "sigmaf"))   // ς
      .add(new Symbol(963, "&sigma;", "sigma"))   // σ
      .add(new Symbol(964, "&tau;", "tau"))   // τ
      .add(new Symbol(965, "&upsilon;", "upsilon"))   // υ
      .add(new Symbol(966, "&phi;", "phi"))   // φ
      .add(new Symbol(967, "&chi;", "chi"))   // χ
      .add(new Symbol(968, "&psi;", "psi"))   // ψ
      .add(new Symbol(969, "&omega;", "omega"))   // ω
      .add(new Symbol(977, "&thetasym;", "Theta symbol"))   // ϑ
      .add(new Symbol(978, "&upsih;", "Upsilon symbol"))   // ϒ
      .add(new Symbol(982, "&piv;", "Pi symbol"))   // ϖ
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableList<Symbol> initMisc()
  {
    return ImmutableList.<Symbol>builder()

      .add(new Symbol(338, "&OElig;", "Uppercase ligature OE"))   // Œ
      .add(new Symbol(339, "&oelig;", "Lowercase ligature OE"))   // œ
      .add(new Symbol(352, "&Scaron;", "Uppercase S with caron"))   // Š
      .add(new Symbol(353, "&scaron;", "Lowercase S with caron"))   // š
      .add(new Symbol(376, "&Yuml;", "Capital Y with diaeres"))   // Ÿ
      .add(new Symbol(402, "&fnof;", "Lowercase with hook"))   // ƒ
      .add(new Symbol(732, "&tilde;", "Tilde"))   // ~
      .add(new Symbol(8211, "&ndash;", "En dash"))   // –
      .add(new Symbol(8212, "&mdash;", "Em dash"))   // —
      .add(new Symbol(8216, "&lsquo;", "Left single quotation mark"))   // ‘
      .add(new Symbol(8217, "&rsquo;", "Right single quotation mark"))   // ’
      .add(new Symbol(8218, "&sbquo;", "Single low-9 quotation mark"))   // ‚
      .add(new Symbol(8220, "&ldquo;", "Left double quotation mark"))   // “
      .add(new Symbol(8221, "&rdquo;", "Right double quotation mark"))   // ”
      .add(new Symbol(8222, "&bdquo;", "Double low-9 quotation mark"))   // „
      .add(new Symbol(8224, "&dagger;", "Dagger"))   // †
      .add(new Symbol(8225, "&Dagger;", "Double dagger"))   // ‡
      .add(new Symbol(8226, "&bull;", "Bullet"))   // •
      .add(new Symbol(8230, "&hellip;", "Horizontal ellipsis"))   // …
      .add(new Symbol(8943, "&#8943;", "Midline horizonal ellipsis"))   // ⋯
      .add(new Symbol(8942, "&vellip;", "Vertical elllipsis"))  // ⋮
      .add(new Symbol(9001, "&#9001;", "Left Angle Bracket"))  // 〈
      .add(new Symbol(9002, "&#9002;", "Right Angle Bracket"))  // 〉
      .add(new Symbol(8249, "&lsaquo;", "Single left angle quotation"))   // ‹
      .add(new Symbol(8250, "&rsaquo;", "Single right angle quotation"))   // ›
      .add(new Symbol(8254, "&oline;", "Overline"))   // ‾
      .add(new Symbol(8364, "&euro;", "Euro"))   // €
      .add(new Symbol(8482, "&trade;", "Trademark"))   // ™
      .add(new Symbol(8629, "&crarr;", "Carriage return arrow"))   // ↵
      .add(new Symbol(9674, "&loz;", "Lozenge"))   // ◊
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
