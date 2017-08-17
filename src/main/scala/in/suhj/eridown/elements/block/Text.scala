package in.suhj.eridown.elements.block

import in.suhj.eridown._

import scala.xml.Utility.escape

case class Text(text: String) extends Element {
    def render = escape(text)
}

object TextGenerator extends Generator {
    override def generators = Nil

    def generate(text: String) = Valid(Text(text), text.length)
}