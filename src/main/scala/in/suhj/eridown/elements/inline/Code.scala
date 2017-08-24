package in.suhj.eridown.elements.inline

import in.suhj.eridown.core._
import xml.Utility.escape

case class CodeInline(text: String) extends Element {
    def render = s"<code>${escape(text)}</code>"
}

object CodeInlineGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads("`")) return None
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find("`")) return None
        val content = scanner.extract

        if (content.isEmpty) None
        else Some((CodeInline(content), scanner.position + 1))
    }
}