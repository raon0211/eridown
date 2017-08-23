package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core.{Element, Generator, ParseResult}

import scala.xml.Utility.escape

case class Text(text: String) extends Element {
    def render = escape(text)
}

object TextGenerator extends Generator {
    def generate(text: String) = Some(ParseResult(Text(text), text.length))
}