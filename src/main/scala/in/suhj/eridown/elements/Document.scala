package in.suhj.eridown.elements

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option._
import in.suhj.eridown.elements.block._

case class Document(val content: String) extends Element {
    def render = content
}

object DocumentGenerator extends Generator {
    override def generators = blocks
    override def fillGenerator = ParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()

    def generate(content: String): Option[ParseResult] =
        Some(ParseResult(parse(content), content.length))
    def parse(content: String): Document =
        Document(transform(content))
}