package in.suhj.eridown.elements.block

import in.suhj.eridown.core._

case class ThematicBreak() extends Element {
    def render = "<hr />"
}

object ThematicBreakGenerator extends Generator {
    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)
        scanner.skip(3)

        if (!List("***", "---", "___").contains(scanner.extract)) None
        else Some((ThematicBreak(), 3))
    }
}
