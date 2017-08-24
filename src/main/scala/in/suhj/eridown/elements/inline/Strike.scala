package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class Strike(text: String) extends Element {
    def render = s"<del>$text</del>"
}

object StrikeGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads("~~")) return None
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find("~~")) return None
        val content = scanner.extract

        if (content.isEmpty) None
        else Some((Strike(transform(content)), scanner.position + 2))
    }
}