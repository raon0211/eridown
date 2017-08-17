package in.suhj.eridown.elements.block

import in.suhj.eridown._

case class Code(language: String, text: String) extends Element {
    def render = s"""<pre class="lang-$language"><code>${text.trim}</code></pre>"""
}

object CodeGenerator extends BlockGenerator {
    override val isBlock = true
    override def generators = Nil

    def generate(content: String): ParseResult = {
        val scanner = Scanner(content)

        if (!scanner.reads("```") || scanner.reads("````"))
            return Invalid()
        scanner.skip(3)

        scanner.mark()
        scanner.skipToLineEnd()
        val lang = scanner.extract
        scanner.skipLineEnd()

        scanner.mark()
        if (!scanner.find("```"))
            return Invalid()
        val text = scanner.extract

        scanner.skip(4)
        Valid(Code(lang, text), scanner.position)
    }
}