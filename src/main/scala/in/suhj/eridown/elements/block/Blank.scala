package in.suhj.eridown.elements.block

import in.suhj.eridown.core._

case class Blank() extends Element {
    def render = ""
}

object BlankGenerator extends Generator {
    override def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)

        scanner.mark()
        scanner.skipToNextLine()
        val content = scanner.extract.trim

        if (content.nonEmpty) None
        else Some((Blank(), scanner.position))
    }
}
