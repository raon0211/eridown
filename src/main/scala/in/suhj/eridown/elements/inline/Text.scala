package in.suhj.eridown.elements.inline

import in.suhj.eridown._

import scala.xml.Utility.escape

case class Text(text: String) extends Element {
    def render = escape(text)
}

object TextGenerator extends InlineGenerator {
    override def generators = Nil

    def generate(text: String) = Valid(Text(text), text.length)
}