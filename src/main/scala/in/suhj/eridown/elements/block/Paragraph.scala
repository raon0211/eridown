package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.elements.inline.TextGenerator

case class Paragraph(text: Seq[String]) extends Element {
    def render = text.map("<p>" + _ + "</p>").mkString
}

object ParagraphGenerator extends BlockGenerator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String) = {
        Valid(Paragraph(text.split("\n\n").map(transform)), text.length)
    }
}