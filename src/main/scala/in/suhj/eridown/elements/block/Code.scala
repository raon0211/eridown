package in.suhj.eridown.elements.block

import in.suhj.eridown._
import in.suhj.eridown.core._

case class Code(language: String, text: String) extends Element {
    def render = s"""<pre class="lang-$language"><code>${text.trim}</code></pre>"""
}

object CodeGenerator extends Generator {
    def generate(content: String): Option[ParseResult] = {
        val scanner = Scanner(content)

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
        val text = scanner.extract

        scanner.skip(3)
        Some(ParseResult(Code(lang, text), scanner.position))
    }
}