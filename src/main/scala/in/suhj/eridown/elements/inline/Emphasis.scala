package in.suhj.eridown.elements.inline

import in.suhj.eridown._

case class Emphasis(text: String) extends Element {
    def render = s"<b>$text</b>"
}

object EmphasisGenerator extends InlineGenerator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads('*')) return Invalid()
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find('*')) return Invalid()
        val content = scanner.extract

        if (content.isEmpty) Invalid()
        else Valid(Emphasis(content), scanner.position + 1)
    }
}