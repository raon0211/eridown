package in.suhj.eridown

import scala.collection.mutable.ListBuffer
import xml.Utility.escape

abstract class Element {
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
    def render = s"<ul>${children.map(_.render).mkString}</ul>"
}

case class ListItem(text: String, indent: Int, ordered: Boolean) extends Element {
    def render = s"<li>$text</li>"
}

case class Text(text: String) extends Element {
    def render = escape(text)
}