package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core.{Element, Generator}
import in.suhj.eridown.elements.inline.TextGenerator

case class Paragraph(text: Seq[String]) extends Element {
    def render = text.map("<p>" + _ + "</p>").mkString
}

object ParagraphGenerator extends Generator {
    def generate(text: String): Option[ParseResult] =
        Some(
            (
                Paragraph(
                    text.split("\n\n").map(line => transform(line.trim)).filter(_.nonEmpty)
                ),
                text.length
            )
        )
}