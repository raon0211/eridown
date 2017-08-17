package in.suhj.eridown

import scala.collection.mutable.ListBuffer
import xml.Utility.escape

trait Element {
    def render: String
}

case class Document(val content: String) extends Element {
    def render = content
}

case class Heading(level: Int, text: String) extends Element {
    def render = s"<h$level>$text</h$level>"
}

case class Blockquote(text: String) extends Element {
    def render = s"<blockquote>$text</blockquote>"
}

case class Code(language: String, text: String) extends Element {
    def render = s"""<pre class="lang-$language"><code>${text.trim}</code></pre>"""
}

case class Paragraph(text: String) extends Element {
    def render = s"<p>$text</p>"
}

case class ListElement(val children: ListBuffer[Element]) extends Element {
    def render = {
        val element = if (children(0).asInstanceOf[ListItem].ordered) "ol" else "ul"
        s"<ul>${children.map(_.render).mkString}</ul>"
    }
}

case class ListItem(text: String, indent: Int, ordered: Boolean) extends Element {
    def render = s"<li>$text</li>"
}

case class Table(text: String) extends Element {
    def render = s"<table>$text</table>"
}

case class TableRow(val children: ListBuffer[TableData]) extends Element {
    def render = s"<tr>${children.map(_.render).mkString}</tr>"
}

object TableDataAlignment extends Enumeration {
    type TableDataAlignment = Value
    val Left, Right, Center = Value
}
import TableDataAlignment._

case class TableData(text: String, alignment: TableDataAlignment, isHeader: Boolean) extends Element {
    var rowspan = 1
    var colspan = 1

    def render = {
        val element = if (isHeader) "th" else "td"
        def attributes = {
            var span = ""

            if (rowspan > 1) span += s""" rowspan="$rowspan""""
            if (colspan > 1) span += s""" colspan="$colspan""""

            if (alignment == Center) span += s""" class="center""""
            else if (alignment == Right) span += s""" class="right""""

            span
        }

        s"""<$element$attributes>$text</$element>"""
    }
}

case class Text(text: String) extends Element {
    def render = escape(text)
}