package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class Image(content: String, link: String) extends Element {
    def render = s"""<img src="$link" alt="$content">"""
}

object ImageGenerator extends Generator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads("![")) return Invalid()
        scanner.skip(2)

        scanner.mark()
        if (!scanner.find(']')) return Invalid()
        val content = scanner.extract
        scanner.skip(1)

        if (!scanner.reads('(')) return Invalid()
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find(')')) return Invalid()
        val link = scanner.extract
        scanner.skip(1)

        Valid(Image(content, link), scanner.position)
    }
}