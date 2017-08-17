package in.suhj.eridown

import in.suhj.eridown.elements.DocumentGenerator

object Parser {
    def render(text: String): String = DocumentGenerator.parse(text.replace("\r\n", "\n")).render
}