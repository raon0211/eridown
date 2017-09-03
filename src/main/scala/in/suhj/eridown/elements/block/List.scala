package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.block.ParagraphGenerator.getIndent
import in.suhj.eridown.elements.inline.TextGenerator
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
            s"${children.map(
                if (tight) _.tightRender
                else _.looseRender
            ).mkString}" +
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
    private var tight = true

    def text =
        if (rawText.length >= textStart)
            rawText.substring(
                Math.min(rawText.length, textStart)
            )
        else ""
    def text_=(newText: String) =
        rawText = rawText.substring(0,
            Math.min(rawText.length, textStart)
        ) + newText

    def render = tightRender

    def looseRender = s"<li>${BlockTransformer.transform(text)}</li>"
    def tightRender = s"<li>${TightTransformer.transform(text)}</li>"

    override def integrate(targets: List[Element]): (Element, Int) = {
        var canIntegrate = true
        var integratedIndex = 0

        val items = new ListBuffer[ListItem] += this

        while (canIntegrate && integratedIndex < targets.length) {
            def addLineToItem(elem: Element) = {
                val prevTarget: Element =
                    if (integratedIndex == 0) this
                    else targets(integratedIndex - 1)

                val isValid: Boolean = elem.indent >= textStart
                val text: String = Scanner.stripLeft(elem.rawText, textStart)

                prevTarget match {
                    case _: Blank => {
                        if (isValid) {
                            tight = false
                            addTextToItem(text)
                        } else canIntegrate = false
                    }
                    case _ => addTextToItem(text)
                }
            }

            def addTextToItem(text: String) = {
                items.last.text = items.last.text + "\n" + text
                integratedIndex += 1
            }

            targets(integratedIndex) match {
                case item: ListItem => {
                    if (delim != item.delim) {
                        return (ListElement(items, tight), integratedIndex + 1)
                    } else if (items.last.textStart <= item.indent) {
                        addTextToItem(item.rawText)
                    } else {
                        items += item
                        integratedIndex += 1
                    }
                }
                case _: Blank => {
                    if (items.last.text.trim.isEmpty) {
                        canIntegrate = false
                    } else {
                        addTextToItem("")
                    }
                }
                case elem => addLineToItem(elem)
            }
        }

        (ListElement(items, tight), integratedIndex + 1)
    }
}

object ListItemGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
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