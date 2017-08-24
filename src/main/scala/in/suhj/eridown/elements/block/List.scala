package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator
import in.suhj.eridown.option.Option.blocks

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ListElement(val children: ListBuffer[Element]) extends Element {
    def render = {
        val element = if (children(0).asInstanceOf[ListItem].ordered) "ol" else "ul"
        s"<$element>${children.map(_.render).mkString}</$element>"
    }
}

case class ListItem(text: String, indent: Int, ordered: Boolean) extends Element {
    def render = s"<li>$text</li>"
}

object ListGenerator extends Generator {
    def generate(content: String): Option[ParseResult] = {
        val items = getChildrenData(ListItemGenerator, content)
        if (items.isEmpty) return None

        def createListHierarchy(items: List[ParseResult]): (Element, Int) = {
            var totalOffset = 0
            val root: ListElement = ListElement(new ListBuffer)

            val nodesMap: mutable.Map[Int, ListBuffer[ListElement]] = mutable.Map()
            nodesMap += -1 -> (new ListBuffer += root)

            for {
                i <- items.indices
                prev = i - 1
            } {
                val (elem, length) = items(i)
                val item = elem.asInstanceOf[ListItem]
                val level = item.indent

                def needsNewList = {
                    if (!nodesMap.contains(item.indent)) true
                    else {
                        val prevLevel = items(i - 1)._1.asInstanceOf[ListItem].indent
                        level > prevLevel
                    }
                }

                val parentKey = nodesMap.keySet.filter(_ < level).max
                val parent = nodesMap(parentKey).last

                if (needsNewList) {
                    val buffer = new ListBuffer[Element] += item
                    nodesMap += level -> (new ListBuffer += ListElement(buffer))
                    parent.children += ListElement(buffer)
                } else {
                    val buffer = nodesMap(level).last.children
                    buffer += item
                    // Since the buffer indicates(shares) the same object, there is no need to,
                    // and should not add the item again to buffer.
                }

                totalOffset += length
            }

            (root.children.head, totalOffset)
        }

        val (root, totalOffset) = createListHierarchy(items)
        Some((root, totalOffset))
    }
}

object ListItemGenerator extends Generator {
    override def generators = blocks
    override def fillGenerator = ParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()

    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)
        var indent = 0

        while (Scanner.isWhitespace(scanner.currentChar)) {
            scanner.skip(1)
            indent += 1
        }

        def isUnordered(): Boolean = {
            if (!scanner.reads("* ") && !scanner.reads("- ")) false
            else {
                scanner.skip(2)
                true
            }
        }
        def isOrdered(): Boolean = {
            val start = scanner.position

            if (!Character.isDigit(scanner.currentChar)) false
            else {
                scanner.skip(1)

                var count = 0
                while (Character.isDigit(scanner.currentChar) && count < 10) {
                    count += 1
                    scanner.skip(1)
                }

                if (count >= 10) return false

                if (!scanner.reads(". ")) {
                    scanner.position = start
                    false
                } else {
                    scanner.skip(2)
                    true
                }
            }
        }

        val unordered = isUnordered()
        val ordered = isOrdered()

        if (!unordered && !ordered) {
            return None
        }

        scanner.skipWhitespace()
        scanner.mark()
        scanner.skipToNextLine()

        val content = scanner.extract.trim
        if (content.isEmpty) return None

        Some((
            ListItem(
                transform(content),
                indent,
                ordered
            ),
            scanner.position
        ))
    }
}