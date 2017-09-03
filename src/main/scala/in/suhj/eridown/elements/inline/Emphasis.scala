package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

case class Emphasis(text: String) extends Element {
    def render = s"<em>$text</em>"
}

object EmphasisGenerator extends Generator {
    def generate(text: String): Option[GenerateResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads('*')) return None
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find('*')) return None
        val content = scanner.extract

        if (content.isEmpty) None
        else Some((Emphasis(InlineTransformer.transform(content)), scanner.position + 1))
    }
}