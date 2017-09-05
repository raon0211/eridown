package in.suhj.eridown.elements.block

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option.blocks

import scala.collection.mutable.ListBuffer

abstract class TextLine(
    val content: String
)  extends Element {
    def render = text

    def text = trimLeft(4)
    def trimLeft(characters: Int): String = {
        if (content.isEmpty) return ""

        var index = 0
        var indent = 0

        while (indent < characters) {
            val currentChar = content(index)
            val skip =
                if (currentChar == ' ') 1
                else if (currentChar == '\t') 4
                else 0

            if (skip == 0) return content.substring(index)

            indent += skip
            index += 1
        }

        content.substring(index)
    }
    val isCode: Boolean = indent >= 4
}

case class CodeLine(
   override val content: String
) extends TextLine(content) {
    override def integrate(targets: List[Element]): IntegrateResult = {
        var canIntegrate = true
        var index = 0

        val content = new ListBuffer[CodeLine] += this

        while (canIntegrate && index < targets.length) {
            targets(index) match {
                case indent: CodeLine => {
                    content += indent
                    index += 1
                }
                case _: Blank => {
                    content += CodeLine("")
                    index += 1
                }
                case _ => canIntegrate = false
            }
        }

        val _content = content.map(_.trimLeft(4)).mkString("\n").replaceAll("\\s+$", "")
        (Code(_content, ""), index + 1)
    }
}

case class Paragraph(
    override val content: String,
    val tight: Boolean
) extends TextLine(content) {
    override def render =
        if (tight) InlineTransformer.transform(content.trim)
        else s"<p>${InlineTransformer.transform(content.trim)}</p>"

    override def integrate(targets: List[Element]): IntegrateResult = {
        var canIntegrate = true
        var index = 0

        var content = new ListBuffer[String] += this.content

        while (canIntegrate && index < targets.length) {
            def canInterrupt(item: ListItem) =
                if (item.text.trim.isEmpty) false
                else if (item.startNum == 1) true
                else if (item.delim == '*' || item.delim == '-' || item.delim == '+') true
                else false

            targets(index) match {
                case line: TextLine => {
                    content += line.text
                    index += 1
                }
                case item: ListItem => {
                    if (!canInterrupt(item)) {
                        content += item.rawText
                        index += 1
                    } else {
                        canIntegrate = false
                    }
                }
                case _ => canIntegrate = false
            }
        }

        (Paragraph(content.map(_.trim).mkString("\n"), tight), index + 1)
    }
}

abstract class TextLineGenerator extends Generator {
    def getIndent(text: String): Int = {
        val scanner = Scanner(text)
        scanner.mark()

        var indent = 0
        var foundNotWhitespace = false

        while (!foundNotWhitespace) {
            val skip =
                if (scanner.currentChar == ' ') 1
                else if (scanner.currentChar == '\t') 4
                else 0

            if (skip != 0) {
                indent += skip
                scanner.skip(1)
            } else foundNotWhitespace = true
        }

        indent
    }
}
object CodeLineGenerator extends TextLineGenerator {
    def generate(text: String) = generate(text, true)

    def generate(text: String, useIndentLine: Boolean): Option[GenerateResult] = {
        val indent = getIndent(text)
        val lines = text.split("\n", -1)
        val line = lines(0)
        val newline = if (lines.length > 1) 1 else 0

        if (line.trim.nonEmpty && indent >= 4) Some((CodeLine(line), line.length + newline))
        else None
    }
}

object ParagraphGenerator extends TextLineGenerator {
    def generate(text: String): Option[GenerateResult] = {
        val indent = getIndent(text)
        val lines = text.split("\n")
        val line = lines(0)
        val newline = if (lines.length > 1) 1 else 0

        Some((Paragraph(line, false), line.length + newline))
    }
}