package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.block.ParagraphGenerator.getIndent
import in.suhj.eridown.elements.inline.{HtmlTag, TextGenerator}
import in.suhj.eridown.option.Option._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ListElement(children: ListBuffer[ListItem], tight: Boolean) extends Element {
    def startNum = children(0).startNum

    def render = {
        val element =
            if (startNum >= 0) "ol"
            else "ul"
        val start = if (startNum >= 0 && startNum != 1) s""" start="$startNum"""" else ""

        s"<$element$start>" +
            children.map(
                if (tight) _.tightRender
                else _.looseRender
            ).mkString +
        s"</$element>"
    }
}

object TightParagraphGenerator extends TextLineGenerator {
    def generate(text: String): Option[GenerateResult] = {
        val indent = getIndent(text)
        val lines = text.split("\n")
        val line = lines(0)
        val newline = if (lines.length > 1) 1 else 0

        Some((Paragraph(line, true), text.length))
    }
}

private object TightTransformer extends Transformer {
    override val generators: List[Generator] = blocks
    override val fillGenerator: Generator = TightParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()
}

case class ListItem(
    textStart: Int,
    startNum: Int,
    delim: Char
) extends Element {
    var children: List[Element] = Nil
    def looseChildren = children.map {
        case Paragraph(text, true) => Paragraph(text, false)
        case child => child
    }

    def content =
        if (rawText.length >= textStart)
            rawText.substring(
                Math.min(rawText.length, textStart)
            )
        else ""
    def content_=(newText: String) =
        rawText = rawText.substring(0,
            Math.min(rawText.length, textStart)
        ) + newText

    def render = tightRender

    def looseRender = s"<li>${looseChildren.map(_.render).mkString}</li>"
    def tightRender = s"<li>${children.map(_.render).mkString}</li>"

    override def integrate(targets: List[Element]): (Element, Int) = {
        var canIntegrate = true
        var index = 0

        val items = new ListBuffer[ListItem] += this
        var tight = true

        def prevTarget: Element =
            if (index == 0) this
            else targets(index - 1)

        while (canIntegrate && index < targets.length) {
            val target = targets(index)

            val isListItem = target.isInstanceOf[ListItem]
            val isBlank = target.isInstanceOf[Blank]
            val isIndentSufficient = target.indent >= textStart

            val prevIsBlank = prevTarget.isInstanceOf[Blank]

            def canContinue = {
                if (prevIsBlank)
                    if (isListItem) {
                        tight = false
                        true
                    }
                    else isBlank || (items.last.content.trim.nonEmpty && isIndentSufficient)
                else target match {
                    case item: ListItem => delim == item.delim
                    case _: Blank => true
                    case _: TextLine => true
                    case _ => isIndentSufficient
                }
            }

            if (canContinue) {
                val text: String = Scanner.stripLeft(target.rawText, textStart)

                target match {
                    case item: ListItem if items.last.textStart <= item.indent =>
                        addTextToItem(text)
                    case item: ListItem => addItemToList(item)
                    case line: CodeLine if items.last.textStart > line.indent =>
                        ListItemGenerator.create(line.text) match {
                            case Some(item) => addItemToList(item.asInstanceOf[ListItem])
                            case None => addTextToItem(text)
                        }
                    case _: Blank => addTextToItem(text)
                    case _ => addTextToItem(text)
                }

                def addItemToList(item: ListItem): Unit = {
                    items += item
                    index += 1
                }

                def addTextToItem(text: String) = {
                    items.last.content = items.last.content + "\n" + text
                    index += 1
                }
            } else canIntegrate = false
        }

        for (item <- items) {
            val children = TightTransformer.parse(item.content)

            if (tight) {
                val blanksBetween = children
                    .dropWhile(_.isInstanceOf[Blank])
                    .reverse.dropWhile(_.isInstanceOf[Blank]).reverse
                    .filter(_.isInstanceOf[Blank])

                if (blanksBetween.nonEmpty) tight = false
            }

            item.children = children
            item
        }

        if (prevTarget.isInstanceOf[Blank]) index -= 1

        (ListElement(items, tight), index + 1)
    }
}

object ListItemGenerator extends Generator {
    protected def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)
        var indent = 0

        var plainText = scanner.aheadLine

        while (Scanner.isWhitespace(scanner.currentChar)) {
            scanner.skip(1)
            indent += 1
        }

        def unorderedListInfo: (Int, Char) =
            if (scanner.reads("*") || scanner.reads("-") || scanner.reads("+")) (1, scanner.currentChar)
            else (-1, '\0')
        val (unorderedMarkerLength, unorderedDelim) = unorderedListInfo

        def orderedListInfo: (Int, Int, Char) = {
            val _scanner = Scanner(scanner.ahead)
            _scanner.mark()

            var count = 0
            while (Character.isDigit(_scanner.currentChar) && count < 10) {
                count += 1
                _scanner.skip(1)
            }

            if (count == 0 || count >= 10) (-1, -1, '\0')
            else {
                if (!_scanner.reads(".") && !_scanner.reads(")")) {
                    (-1, -1, '\0')
                } else {
                    val startNum = _scanner.extract.toInt
                    val delim = _scanner.currentChar
                    _scanner.skip(1)

                    (_scanner.position, startNum, delim)
                }
            }
        }
        val (orderedMarkerLength, orderedStartNum, orderedDelim) = orderedListInfo

        val unordered = unorderedDelim != '\0'
        val ordered = orderedMarkerLength >= 0

        if (!unordered && !ordered) {
            return None
        }

        val delim =
            if (unordered) unorderedDelim
            else orderedDelim

        val skip = if (unordered) unorderedMarkerLength else orderedMarkerLength
        scanner.skip(skip)

        val line = plainText.substring(skip)
        var afterWhitespace = 0

        if (line.trim.nonEmpty) {
            if (scanner.currentChar != ' ') return None
            scanner.skip(1)

            while (scanner.atWhitespace) {
                scanner.skip(1)
                afterWhitespace += 1
            }
            if (afterWhitespace >= 4) afterWhitespace = 0
        } else {
            plainText += " "
        }

        val firstTextIndex =
            indent + Math.max(orderedMarkerLength, unorderedMarkerLength) + 1 + afterWhitespace

        scanner.skipToNextLine()

        Some((
            ListItem(
                firstTextIndex,
                Math.max(-1, orderedStartNum),
                delim
            ),
            scanner.position
        ))
    }
}