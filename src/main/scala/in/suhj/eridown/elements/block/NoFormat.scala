package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class NoFormat(text: String) extends Element {
    def render = escape(text)
}

object NoFormatGenerator extends Generator {
    def generate(content: String): Option[GenerateResult] = {
        val scanner = Scanner(content)

        if (!scanner.reads("%%%")) return None
        scanner.skip(3)
        scanner.mark()

        if (!scanner.find("%%%")) return None

        Some((NoFormat(scanner.extract), scanner.position + 3))
    }
}
