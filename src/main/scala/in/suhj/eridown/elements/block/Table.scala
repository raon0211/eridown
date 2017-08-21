package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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
import in.suhj.eridown.elements.block.TableDataAlignment._

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

object TableGenerator extends Generator {
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
    def generate(text: String): ParseResult = {
        val result = getChildrenData(TableDataGenerator, text).map(_.element.asInstanceOf[TableData])
        if (result.isEmpty) return Invalid()

        Valid(TableRow(new ListBuffer[TableData] ++= result), text.indexOf('\n') + 1)
    }
}

object TableDataGenerator extends Generator {
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
