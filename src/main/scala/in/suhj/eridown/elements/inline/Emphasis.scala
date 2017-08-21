package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

case class Emphasis(text: String) extends Element {
    def render = s"<em>$text</em>"
}

object EmphasisGenerator extends Generator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads('*')) return Invalid()
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find('*')) return Invalid()
        val content = scanner.extract

        if (content.isEmpty) Invalid()
        else Valid(Emphasis(transform(content)), scanner.position + 1)
    }
}