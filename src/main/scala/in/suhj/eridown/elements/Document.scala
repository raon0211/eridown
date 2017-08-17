package in.suhj.eridown.elements

import in.suhj.eridown._
import in.suhj.eridown.elements.block.ParagraphGenerator

case class Document(val content: String) extends Element {
    def render = content
}

object DocumentGenerator extends MarkdownGenerator {
    override val isBlock = false
    override def generators = blocks
    override def fillGenerator = ParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()

    def generate(content: String): ParseResult = Valid(parse(content), content.length)
    def parse(content: String): Document = Document(transform(content))
}
