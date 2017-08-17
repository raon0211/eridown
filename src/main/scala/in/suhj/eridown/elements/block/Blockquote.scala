package in.suhj.eridown.elements.block

import in.suhj.eridown._

case class Blockquote(text: String) extends Element {
    def render = s"<blockquote>$text</blockquote>"
}

object BlockquoteGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(content: String) = {
        val scanner = Scanner(content)

        if (!scanner.reads(">")) Invalid()
        else {
            scanner.skip(1)
            scanner.skipWhitespace()

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract

            Valid(Blockquote(transform(text)), scanner.position)
        }
    }
}