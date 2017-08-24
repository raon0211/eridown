package in.suhj.eridown.elements.block

import in.suhj.eridown.core._
import xml.Utility.escape

case class Code(language: String, text: String) extends Element {
    def render = {
        val lang = if (language.nonEmpty) s""" class="lang-$language"""" else ""
        s"""<pre$lang><code>${escape(text)}</code></pre>"""
    }
}

object CodeGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val tabbedCode = CodeTabbedGenerator.generate(text)
        if (tabbedCode != None) return tabbedCode

        val fencedCode = CodeFencedGenerator.generate(text)
        if (fencedCode != None) return fencedCode

        None
    }
}

case class CodeLine(content: String) extends Element {
    def render = content
}

object CodeTabbedGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val lines = getChildrenData(CodeTabbedLineGenerator, text)
        if (lines.isEmpty) return None

        val content = lines.map(_._1.asInstanceOf[CodeLine].content).mkString("\n")
        val totalLength = lines.map(_._2).foldLeft(0)(_ + _)
        Some((Code("", content), totalLength))
    }
}

object CodeTabbedLineGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        var indent = 0
        var done = false
        while (indent < 4 && !done) {
            if (scanner.currentChar == ' ') indent += 1
            else if (scanner.currentChar == '\t') indent += 4
            else done = true

            scanner.skip(1)
        }

        if (done) return None

        scanner.mark()
        scanner.skipToNextLine()
        val content = scanner.extract.trim

        if (content.nonEmpty) Some((CodeLine(content), scanner.position))
        else None
    }
}

object CodeFencedGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads("```") || scanner.reads("````"))
            return None
        scanner.skip(3)

        scanner.mark()
        scanner.skipToLineEnd()
        val lang = scanner.extract
        scanner.skipLineEnd()

        scanner.mark()
        if (!scanner.find("```"))
            return None
        val content = scanner.extract

        scanner.skip(3)
        Some((Code(lang, content), scanner.position))
    }
}