package in.suhj.eridown.elements.block

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option._

case class Blockquote(text: String) extends Element {
    def render = s"<blockquote>$text</blockquote>"
}

object BlockquoteGenerator extends Generator {
    override def generators = blocks
    override def fillGenerator = ParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()

    def generate(content: String): Option[ParseResult] = {
        val scanner = Scanner(content)
        scanner.skipWhitespace()

        if (!scanner.reads(">")) None
        else {
            scanner.skip(1)
            if (scanner.reads(" ")) scanner.skip(1)

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract

            Some((Blockquote(transform(text)), scanner.position))
        }
    }
}

