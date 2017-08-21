package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._
import in.suhj.eridown.elements.inline.TextGenerator

case class Heading(level: Int, text: String) extends Element {
    def render = s"<h$level>$text</h$level>"
}

object HeadingGenerator extends Generator {
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
