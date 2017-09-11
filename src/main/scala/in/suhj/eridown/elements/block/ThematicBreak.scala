package in.suhj.eridown.elements.block

import in.suhj.eridown.core._

case class ThematicBreak() extends Element {
    def render = "<hr />"
}

object ThematicBreakGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)
        scanner.skipWhitespace()
        scanner.mark()
        val tick = scanner.currentChar
        var ticks = 0

        if (!(tick == '*' || tick == '-' || tick == '_')) return None

        var canContinue = true

        while (canContinue && !scanner.atLineEnd) {
            val curr = scanner.currentChar

            if (curr == tick) {
                ticks += 1
                scanner.skip(1)
            } else if (curr == ' ') {
                scanner.skip(1)
            } else canContinue = false
        }

        if (!scanner.atLineEnd) None
        else if (ticks < 3) None
        else Some((ThematicBreak(), scanner.position))
    }
}
