package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator

import scala.collection.mutable.ListBuffer

case class DefinitionList(children: List[Element]) extends Element {
    def render = s"<dl>${children.map(_.render).mkString}</dl>"
}

case class DefinitionItem(text: String, isTerm: Boolean) extends Element {
    def render = {
        val element = if (isTerm) "dt" else "dd"
        s"<$element>$text</$element>"
    }
}

object DefinitionListGenerator extends Generator {
    def generate(content: String): Option[GenerateResult] = {
        val children = getChildrenData(DefinitionItemGenerator, content)
        if (children.isEmpty) return None

        val items = new ListBuffer[Element]
        var totalOffset = 0

        for (child <- children) {
            items += child
            totalOffset += child.length
        }

        Some((DefinitionList(items.toList), totalOffset))
    }
}

object DefinitionItemGenerator extends Generator {
    def generate(content: String): Option[GenerateResult] = {
        val scanner = Scanner(content)

        val isTerm = scanner.reads(';')
        val isDefinition = scanner.reads(':')
        if (!isTerm && !isDefinition) return None

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

        Some((DefinitionItem(text, isTerm), scanner.position))
    }
}