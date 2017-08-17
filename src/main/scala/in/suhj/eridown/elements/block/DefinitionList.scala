package in.suhj.eridown.elements.block

import in.suhj.eridown._

import scala.collection.mutable.ListBuffer

case class DefinitionList(val children: List[Element]) extends Element {
    def render = s"<dl>${children.map(_.render).mkString}</dl>"
}

case class DefinitionItem(text: String, isTerm: Boolean) extends Element {
    def render = {
        val element = if (isTerm) "dt" else "dd"
        s"<$element>$text</$element>"
    }
}

object DefinitionListGenerator extends Generator {
    override def generators = List(DefinitionItemGenerator)

    def generate(content: String): ParseResult = {
        val children = getChildrenData(DefinitionItemGenerator, content)
        if (children.isEmpty) return Invalid()

        var totalOffset = 0
        val items = children.map((child) => {
            totalOffset += child.rawLength
            child.element
        })

        Valid(DefinitionList(items), totalOffset)
    }
}

object DefinitionItemGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(content: String): ParseResult = {
        val scanner = Scanner(content)

        val isTerm = scanner.reads(';')
        val isDefinition = scanner.reads(':')
        if (!isTerm && !isDefinition) return Invalid()

        scanner.skip(1)
        scanner.skipWhitespace()

        scanner.mark()
        while (!scanner.readsAny(List(':', ';')) && !scanner.atLineEnd) {
            scanner.skip(1)
        }
        val text = scanner.extract

        if (scanner.atLineEnd) {
            scanner.skipLineEnd()
        }

        Valid(DefinitionItem(text, isTerm), scanner.position)
    }
}