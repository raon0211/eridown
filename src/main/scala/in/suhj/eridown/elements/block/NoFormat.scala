package in.suhj.eridown.elements.block

import in.suhj.eridown._
import xml.Utility.escape

case class NoFormat(text: String) extends Element {
    def render = escape(text)
}

object NoFormatGenerator extends BlockGenerator {
    def generate(content: String): ParseResult = {
        val scanner = Scanner(content)

        if (!scanner.reads("%%%")) return Invalid()
        scanner.skip(3)
        scanner.mark()

        if (!scanner.find("%%%")) return Invalid()

        Valid(NoFormat(scanner.extract), scanner.position + 3)
    }
}
