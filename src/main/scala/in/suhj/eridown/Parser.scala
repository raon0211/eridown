package in.suhj.eridown

object Parser {
    def render(text: String): String = DocumentGenerator.parse(text).render
}