package in.suhj.eridown

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

abstract class ParseResult
case class Valid(element: Element, rawLength: Int) extends ParseResult
case class Invalid() extends ParseResult

case class Range(start: Int, end: Int) {
    def isEmpty = start == end
}
case class ElementRange(element: Element, range: Range)

abstract class Generator {
    protected final def blocks: List[Generator] = List(
        HeadingGenerator,
        BlockquoteGenerator,
        CodeGenerator,
        ListGenerator,
        TableGenerator
    )
    protected final def inlines: List[Generator] = Nil

    protected def generators: List[Generator] = blocks
    protected def fillGenerator: Generator = ParagraphGenerator

    protected def generate(text: String): ParseResult

    protected final def getChildrenData(generator: Generator, text: String): List[Valid] = {
        @tailrec
        def readChild(items: List[Valid], text: String): List[Valid] = {
            generator.generate(text) match {
                case Invalid() => items.reverse
                case valid @ Valid(_, length) => {
                    readChild(valid :: items, text.substring(length))
                }
            }
        }
        readChild(Nil, text)
    }
    protected final def transform(text: String): String = {
        def fillRender(text: String) = fillGenerator.generate(text).asInstanceOf[Valid].element.render

        if (generators.isEmpty) return fillRender(text)

        val elements = new ListBuffer[ElementRange]()
        val scanner = Scanner(text)

        while (!scanner.atEnd) {
            @tailrec
            def generate(generators: List[Generator]): ElementRange =
                generators.head.generate(scanner.ahead) match {
                    case Valid(elem, length) => ElementRange(elem, Range(scanner.position, scanner.position + length))
                    case Invalid() => generate(generators.tail)
                }

            try {
                val result = generate(generators)
                elements += result

                scanner.position = result.range.end
            } catch {
                case _: NoSuchElementException => scanner.position += 1
            }

            scanner.skipLineEnd()
        }

        if (elements.isEmpty) fillRender(text)

        var renders = new ListBuffer[String]

        for {
            i <- elements.indices
            prev = i - 1
        } {
            val ElementRange(element, Range(start, _)) = elements(i)
            val formerEnd =
                if (prev < 0) 0
                else elements(prev).range.end

            val precedingText = text.substring(formerEnd, start)

            if (!precedingText.trim.isEmpty) {
                renders += fillRender(precedingText)
            }

            renders += element.render
        }

        renders.mkString
    }
}

object DocumentGenerator extends Generator {
    override def generators = blocks

    def generate(content: String): ParseResult = Valid(parse(content), content.length)
    def parse(content: String): Document = Document(transform(content))
}

object HeadingGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(content: String): ParseResult = {
        val scanner = Scanner(content)

        if (scanner.currentChar != '#') Invalid()
        else {
            var level: Int = 0

            while (scanner.currentChar == '#' && level <= 6) {
                level = level + 1
                scanner.skip(1)
            }

            scanner.skipWhitespace()

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract

            Valid(Heading(level, transform(text)), scanner.position)
        }
    }
}

object BlockquoteGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(content: String) = {
        val scanner = Scanner(content)

        if (!scanner.reads(">")) Invalid()
        else {
            scanner.skip(1)
            scanner.skipWhitespace()

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract

            Valid(Blockquote(transform(text)), scanner.position)
        }
    }
}

object CodeGenerator extends Generator {
    override def generators = Nil

    def generate(content: String): ParseResult = {
        val scanner = Scanner(content)

        if (!scanner.reads("```") || scanner.reads("````"))
            return Invalid()
        scanner.skip(3)

        scanner.mark()
        scanner.skipToLineEnd()
        val lang = scanner.extract
        scanner.skipLineEnd()

        scanner.mark()
        if (!scanner.find("```"))
            return Invalid()
        val text = scanner.extract

        scanner.skip(4)
        Valid(Code(lang, text), scanner.position)
    }
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
                    // Since the buffer indicates(shares) the same object, there is no need,
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

object TableGenerator extends Generator {
    override def generators = List(TableRowGenerator)

    def generate(text: String): ParseResult = {
        val result = getChildrenData(TableRowGenerator, text)
        if (result.isEmpty) return Invalid()

        val rows = new ListBuffer[TableRow]
        var totalOffset = 0

        for (res <- result) {
            rows += res.element.asInstanceOf[TableRow]
            totalOffset += res.rawLength
        }

        def setSpans(rows: ListBuffer[TableRow]): List[TableRow] = {
            @tailrec
            def countColspan(colspan: Int, row: TableRow, column: Int): Int = {
                if (column == row.children.length - 1) return colspan

                if (!row.children(column + 1).text.trim.isEmpty) colspan
                else countColspan(colspan + 1, row, column + 1)
            }

            @tailrec
            def countRowspan(rowspan: Int, rows: ListBuffer[TableRow], row: Int, column: Int): Int = {
                if (row == rows.length - 1) return rowspan

                if (rows(row + 1).children(column).text.trim != ":::") rowspan
                else countRowspan(rowspan + 1, rows, row + 1, column)
            }

            for (i <- rows.indices) {
                val row = rows(i)

                val newColumns = new ListBuffer[TableData]
                for (j <- row.children.indices) {
                    val column = row.children(j)
                    val text = column.text.trim

                    if (text != ":::" && !text.isEmpty) {
                        column.colspan = countColspan(1, row, j)
                        column.rowspan = countRowspan(1, rows, i, j)
                        newColumns += column
                    }
                }

                rows(i) = TableRow(newColumns)
            }

            rows.toList
        }

        Valid(Table(setSpans(rows).map(_.render).mkString), totalOffset)
    }
}

object TableRowGenerator extends Generator {
    override def generators = List(TableDataGenerator)

    def generate(text: String): ParseResult = {
        val result = getChildrenData(TableDataGenerator, text).map(_.element.asInstanceOf[TableData])
        if (result.isEmpty) return Invalid()

        Valid(TableRow(new ListBuffer[TableData] ++= result), text.indexOf('\n') + 1)
    }
}

object TableDataGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String): ParseResult = {
        import TableDataAlignment._

        val scanner = Scanner(text)

        val delim = scanner.currentChar
        if (delim != '|' && delim != '^')
            return Invalid()
        scanner.skip(1)

        var leadingSpaces = 0
        while (scanner.atWhitespace) {
            scanner.skip(1)
            leadingSpaces += 1
        }
        scanner.mark()

        val seek = scanner.seekAny(List('|', '^'))
        if (seek < 0) return Invalid()
        else if (scanner.seek('\n') < seek) return Invalid()
        scanner.skip(seek - 1)

        var trailingSpaces = 0
        while (scanner.atWhitespace) {
            scanner.skip(-1)
            trailingSpaces += 1
        }
        scanner.skip(1)

        val alignment =
            if (Math.min(leadingSpaces, trailingSpaces) >= 2) Center
            else if (leadingSpaces >= 2) Right
            else Left

        Valid(TableData(scanner.extract, alignment, delim == '^'), scanner.position + trailingSpaces)
    }
}

object ParagraphGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String) = Valid(Paragraph(transform(text)), text.length)
}

object TextGenerator extends Generator {
    override def generators = Nil

    def generate(text: String) = Valid(Text(text), text.length)
}