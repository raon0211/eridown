package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class Link(content: String, link: String) extends Element {
    def render = s"""<a href="$link">$content</a>"""
}

object LinkGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads('[')) return None
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find(']')) return None
        val content = scanner.extract
        scanner.skip(1)

        if (!scanner.reads('(')) return None
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find(')')) return None
        val link = scanner.extract
        scanner.skip(1)

        Some(ParseResult(Link(transform(content), link), scanner.position))
    }
}