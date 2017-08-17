package in.suhj.eridown.elements.block

import in.suhj.eridown._

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
    override def generators = List(ListItemGenerator)

    def generate(content: String): ParseResult = {
        val items = getChildrenData(ListItemGenerator, content)

        if (items.isEmpty) return Invalid()

        def createListHierarchy(items: List[Valid]): (Element, Int) = {
            var totalOffset = 0
            val root: ListElement = ListElement(new ListBuffer)

            val nodesMap: mutable.Map[Int, ListBuffer[ListElement]] = mutable.Map()
            nodesMap += -1 -> (new ListBuffer += root)

            for {
                i <- items.indices
                prev = i - 1
            } {
                val Valid(elem, length) = items(i)
                val item = elem.asInstanceOf[ListItem]
                val level = item.indent

                def needsNewList = {
                    if (!nodesMap.contains(item.indent)) true
                    else {
                        val prevLevel = items(i - 1).element.asInstanceOf[ListItem].indent
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
        Valid(root, totalOffset)
    }
}

object ListItemGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)
        var indent = 0

        while (Scanner.isWhitespace(scanner.currentChar)) {
            scanner.skip(1)
            indent += 1
        }

        def isUnordered() = {
            if (scanner.currentChar != '*' && scanner.currentChar != '-') false
            else {
                scanner.skip(1)
                true
            }
        }
        def isOrdered() = {
            val start = scanner.position

            if (!Character.isDigit(scanner.currentChar)) false
            else {
                scanner.skip(1)

                while (Character.isDigit(scanner.currentChar)) {
                    scanner.skip(1)
                }

                if (!scanner.reads(".")) {
                    scanner.position = start
                    false
                } else {
                    scanner.skip(1)
                    true
                }
            }
        }

        val unordered = isUnordered()
        val ordered = isOrdered()

        if (!unordered && !ordered) {
            return Invalid()
        }

        scanner.skipWhitespace()
        scanner.mark()
        scanner.skipToLineEnd()

        val skipNewline =
            if (scanner.currentChar == '\n' || scanner.currentChar == '\r') 1
            else 0
        Valid(ListItem(transform(scanner.extract), indent, ordered), scanner.position + skipNewline)
    }
}