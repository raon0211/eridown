package in.suhj.eridown.elements.inline

import in.suhj.eridown._

import xml.Utility.escape

case class NoFormatInline(text: String) extends Element {
    def render = escape(text)
}

object NoFormatInlineGenerator extends InlineGenerator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads("%%")) return Invalid()
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find("%%")) return Invalid()
        val content = scanner.extract

        if (content.isEmpty) Invalid()
        else Valid(NoFormatInline(content), scanner.position + 2)
    }
}