package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

import xml.Utility.escape

case class Image(content: String, link: String) extends Element {
    def render = s"""<img src="$link" alt="$content">"""
}

object ImageGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads("![")) return None
        scanner.skip(2)

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

        Some((Image(content, link), scanner.position))
    }
}