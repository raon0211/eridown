package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator

case class Blockquote(text: String) extends Element {
    def render = s"<blockquote>$text</blockquote>"
}

object BlockquoteGenerator extends Generator {
    def generate(content: String): Option[ParseResult] = {
        val scanner = Scanner(content)

        if (!scanner.reads(">")) None
        else {
            scanner.skip(1)
            scanner.skipWhitespace()

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract

            Some((Blockquote(transform(text)), scanner.position))
        }
    }
}