package in.suhj.eridown.elements.block

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option.blocks

import scala.collection.mutable.ListBuffer
import xml.Utility.escape

case class Code(content: String, language: String) extends Element {
    def render = {
        val lang = if (language.nonEmpty) s""" class="language-$language"""" else ""
        s"""<pre$lang><code>${escape(content)}</code></pre>"""
    }
}

object CodeFencedGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)

        var indent = 0
        while (scanner.atWhitespace) {
            scanner.skip(1)
            indent += 1
        }

        val opening =
            if (scanner.reads("```")) {
                scanner.mark()
                scanner.skip(3)
                while (scanner.currentChar == '`') {
                    scanner.skip(1)
                }
                scanner.extract
            } else if (scanner.reads("~~~")) {
                scanner.mark()
                scanner.skip(3)
                while (scanner.currentChar == '~') {
                    scanner.skip(1)
                }
                scanner.extract
            } else ""

        if (opening.isEmpty) return None

        scanner.mark()
        scanner.findAny(List(' ', '\n'))
        val lang = scanner.extract.trim
        scanner.skipToNextLine()

        val content = new ListBuffer[String]

        var codeContinued = true
        while (codeContinued && !scanner.atEnd) {
            var i = 0
            while (i < indent && scanner.atWhitespace) {
                scanner.skip(1)
            }
            val line = scanner.aheadLine
            val closingFenceIndex = line.indexOf(opening)

            if (closingFenceIndex >= 0 && line.substring(0, closingFenceIndex).trim.isEmpty) {
                codeContinued = false
            } else {
                content += line
            }
            scanner.skipToNextLine()
        }
        Some((Code(content.mkString("\n"), lang), scanner.position))
    }
}