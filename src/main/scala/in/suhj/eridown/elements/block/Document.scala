package in.suhj.eridown.elements.block

import in.suhj.eridown._

case class Document(val content: String) extends Element {
    def render = content
}

object DocumentGenerator extends Generator {
    override def generators = blocks

    def generate(content: String): ParseResult = Valid(parse(content), content.length)
    def parse(content: String): Document = Document(transform(content))
}
