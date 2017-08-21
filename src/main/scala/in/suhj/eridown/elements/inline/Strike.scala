package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class Strike(text: String) extends Element {
    def render = s"<del>$text</del>"
}

object StrikeGenerator extends Generator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads("~~")) return Invalid()
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find("~~")) return Invalid()
        val content = scanner.extract

        if (content.isEmpty) Invalid()
        else Valid(Strike(transform(content)), scanner.position + 2)
    }
}