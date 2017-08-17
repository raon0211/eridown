package in.suhj.eridown.elements.block

import in.suhj.eridown._

case class Paragraph(text: String) extends Element {
    def render = s"<p>$text</p>"
}

object ParagraphGenerator extends Generator {
    override def generators = inlines
    override def fillGenerator = TextGenerator

    def generate(text: String) = Valid(Paragraph(transform(text)), text.length)
}