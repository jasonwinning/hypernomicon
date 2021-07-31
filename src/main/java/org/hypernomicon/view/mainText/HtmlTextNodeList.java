/*
 * Copyright 2015-2021 Jason Winning
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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableInt;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.nodes.TextNode;

public class HtmlTextNodeList
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class HtmlTextNode
  {
    private int startNdx, len;
    private String text;
    private TextNode textNode;

  //---------------------------------------------------------------------------

    public HtmlTextNode(String text, TextNode textNode, int startNdx)
    {
      this.text = text;
      this.textNode = textNode;
      this.startNdx = startNdx;
      len = text.length();
    }

    public String getText()       { return text; }
    public int getStartNdx()      { return startNdx; }
    public TextNode getTextNode() { return textNode; }

  //---------------------------------------------------------------------------

    TextNode updateStartNdx(int newStartNdx, boolean split)
    {
      int offset = newStartNdx - startNdx;

      if (split)
        textNode = textNode.splitText(offset);

      text = text.substring(offset);
      startNdx = newStartNdx;
      len -= offset;

      return textNode;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<HtmlTextNode> nodes;
  private final StringBuilder plainText;

  @Override public String toString() { return plainText.toString(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HtmlTextNodeList(Element element)
  {
    nodes = new ArrayList<>();
    plainText = new StringBuilder();

    MutableInt textNdx = new MutableInt(0);

    assignSB(plainText, element.wholeText());

    addNodes(element, textNdx, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addNodes(Element element, MutableInt textNdx, boolean skip)
  {
    skip = skip || skipElement(element);

    for (Node child : element.childNodes())
    {
      if (child instanceof TextNode)
      {
        TextNode textNode = (TextNode)child;
        String nodeText = textNode.getWholeText();

        if (ultraTrim(nodeText).isBlank() == false)
        {
          HtmlTextNode node = new HtmlTextNode(nodeText, textNode, plainText.indexOf(nodeText, textNdx.intValue()));

          if (skip)
          {
            plainText.replace(node.startNdx, node.startNdx + node.len, "");
          }
          else
          {
            textNdx.setValue(node.startNdx + node.len);
            nodes.add(node);
          }
        }
      }
      else if (child instanceof Element)
        addNodes((Element)child, textNdx, skip);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean skipElement(Element element)
  {
    return (element.tagName().equalsIgnoreCase("summary") || // Don't create any keyword links within collapsible headings
            element.tagName().equalsIgnoreCase("a")       || // Don't create any keyword links within anchor tags (they already link to somewhere)
            element.hasAttr(NO_LINKS_ATTR));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  List<HtmlTextNode> getLinkNodes(int startNdx, int endNdx)
  {
    List<HtmlTextNode> linkNodes = new ArrayList<>();

    for (HtmlTextNode node : nodes)
    {
      if (node.startNdx >= endNdx) break;

      if (startNdx < (node.startNdx + node.len))
        linkNodes.add(node);
    }

    if (linkNodes.isEmpty())
      messageDialog("Internal error #47690", mtError);

    return linkNodes;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
