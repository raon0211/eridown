package in.suhj.eridown.elements.block

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option._

import scala.collection.mutable.ListBuffer

case class Blockquote(text: String) extends Element {
    def render = s"<blockquote>${BlockTransformer.transform(text.trim)}</blockquote>"
}

case class BlockquoteLine(text: String) extends Element {
    def render = text

    override def integrate(targets: List[Element]): IntegrateResult = {
        var canIntegrate = true
        var index = 0
        val content = new ListBuffer[String] += this.text

        while (canIntegrate && index < targets.length) {
            targets(index) match {
                case BlockquoteLine(text) => {
                    content += text
                    index += 1
                }
                case Paragraph(text, _) => {
                    content += text
                    index += 1
                }
                case _ => {
                    canIntegrate = false
                }
            }
        }

        (Blockquote(content.mkString("\n")), index + 1)
    }
}

object BlockquoteLineGenerator extends Generator {
    override def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)
        scanner.skipWhitespace()

        if (!scanner.reads(">")) None
        else {
            scanner.skip(1)

            if (scanner.reads(" ")) scanner.skip(1)

            scanner.mark()
            scanner.skipToLineEnd()
            val text = scanner.extract
            scanner.skipLineEnd()

            Some((BlockquoteLine(text), scanner.position))
        }
    }
}
