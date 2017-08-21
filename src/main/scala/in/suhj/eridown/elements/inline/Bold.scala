package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

case class Bold(text: String) extends Element {
    def render = s"<b>$text</b>"
}

object BoldGenerator extends Generator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads("**")) return Invalid()
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find("**")) return Invalid()
        val content = scanner.extract

        if (content.isEmpty) Invalid()
        else Valid(Bold(transform(content)), scanner.position + 2)
    }
}