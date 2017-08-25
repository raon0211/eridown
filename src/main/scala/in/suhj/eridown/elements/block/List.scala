package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator
import in.suhj.eridown.option.Option._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Lists(val lists: List[ListElement]) extends Element {
    def render = lists.map(_.render).mkString
}

case class ListElement(val children: ListBuffer[Element]) extends Element {
    def startNum = children(0).asInstanceOf[ListItem].startNum

    def render = {
        val element = {
            if (startNum >= 1) {
                if (startNum == 1) "ol"
                else s"""ol start="${startNum}""""
            }
            else "ul"
        }
        s"<$element>${children.map(_.render).mkString}</$element>"
    }
}

case class ListItem(text: String, indent: Int, startNum: Int) extends Element {
    def render = s"<li>$text</li>"
}

object ListGenerator extends Generator {
    def generate(content: String): Option[ParseResult] = {
        val children = getChildrenData(ListItemGenerator, content)
        if (children.isEmpty) return None

        val lists = new ListBuffer[ListElement] += ListElement(new ListBuffer[Element] += children.head._1)

        for (child <- children.tail) {
            val currItem = child._1.asInstanceOf[ListItem]
            val isCurrItemOrdered = currItem.startNum >= 0
            val isCurrListOrdered = lists.last.startNum >= 0

            if (isCurrItemOrdered == isCurrListOrdered) {
                lists.last.children += currItem
            } else {
                lists += ListElement(new ListBuffer[Element] += currItem)
            }
        }

        val totalOffset = children.map(_._2).sum

        Some((Lists(lists.toList), totalOffset))
    }
}

object ListItemGenerator extends Generator {
    private var _generators = blocks
    private var _fillGenerator: Generator = ParagraphGenerator
    private var _skipToNext: Scanner => Unit = scanner => scanner.skipToNextLine()

    override def generators = _generators
    override def fillGenerator = _fillGenerator
    override def skipToNext(scanner: Scanner) = _skipToNext(scanner)

    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)
        var indent = 0

        while (Scanner.isWhitespace(scanner.currentChar)) {
            scanner.skip(1)
            indent += 1
        }

        def isUnordered: Boolean = {
            if (!scanner.reads("* ") && !scanner.reads("- ")) false
            else {
                scanner.skip(2)
                true
            }
        }
        val unorderedMarkerLength = 2

        def orderedListInfo: (Int, Int) = {
            val start = scanner.position

            scanner.mark()
            var count = 0
            while (Character.isDigit(scanner.currentChar) && count < 10) {
                count += 1
                scanner.skip(1)
            }

            if (count == 0 || count >= 10) (-1, -1)
            else {
                if (!scanner.reads(". ")) {
                    scanner.position = start
                    (-1, -1)
                } else {
                    val startNum = scanner.extract.toInt
                    scanner.skip(2)

                    (count + 2, startNum)
                }
            }
        }
        val (orderedMarkerLength, orderedStartNum) = orderedListInfo

        val unordered = isUnordered
        val ordered = orderedMarkerLength >= 0

        if (!unordered && !ordered) {
            return None
        }

        var afterWhitespace = 0
        while (scanner.atWhitespace) {
            scanner.skip(1)
            afterWhitespace += 1
        }

        val firstTextIndex = Math.max(orderedMarkerLength, unorderedMarkerLength) + afterWhitespace + indent

        scanner.skipWhitespace()
        scanner.mark()
        scanner.skipToNextLine()

        val lines = new ListBuffer[String] += scanner.extract.trim
        var reading = true

        def validLine(line: String): Boolean = {
            if (line.isEmpty) true
            else {
                val indentStr = List.fill(firstTextIndex)(" ").mkString
                if (line.startsWith(indentStr)) true
                else false
            }
        }

        while (reading && !scanner.atEnd) {
            val startPos = scanner.position

            scanner.mark()
            scanner.skipToLineEnd()
            val line = scanner.extract
            scanner.skipLineEnd()

            if (validLine(line)) {
                lines += (line match {
                    case "" => ""
                    case _ => line.substring(firstTextIndex)
                })
            } else {
                scanner.position = startPos
                reading = false
            }
        }

        if (lines.last.isEmpty) lines.remove(lines.length - 1)

        if (lines.length == 1) {
            _generators = inlines
            _fillGenerator = TextGenerator
            _skipToNext = scanner => scanner.skip(1)
        } else {
            _generators = blocks
            _fillGenerator = ParagraphGenerator
            _skipToNext = scanner => scanner.skipToNextLine()
        }

        val content = lines.mkString("\n").trim
        if (content.isEmpty) return None
        Some((
            ListItem(
                transform(content),
                indent,
                Math.max(-1, orderedStartNum)
            ),
            scanner.position
        ))
    }
}