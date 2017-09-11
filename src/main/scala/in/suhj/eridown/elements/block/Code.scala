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

case class CodeFence(fence: String, language: String) extends Element {
    def render = fence

    override def integrate(targets: List[Element]): IntegrateResult = {
        val content = new ListBuffer[String]

        var canIntegrate = true
        var index = 0

        while (canIntegrate && index < targets.length) {
            val target = targets(index)

            target match {
                case CodeFence(text, "") => canIntegrate = false
                case item => content += item.rawText
            }

            index += 1
        }

        (Code(content.mkString("\n"), language), index + 1)
    }
}

object CodeFenceGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)

        var indent = 0
        while (scanner.atWhitespace) {
            scanner.skip(1)
            indent += 1
        }

        val fence =
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

        if (fence.isEmpty) return None

        scanner.mark()
        scanner.findAny(List(' ', '\n'))
        val lang = scanner.extract.trim
        scanner.skipToNextLine()

        Some(CodeFence(fence, lang), scanner.position)
    }
}