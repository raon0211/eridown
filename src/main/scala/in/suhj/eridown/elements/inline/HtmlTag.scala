package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core._

case class HtmlTag(content: String) extends Element {
    def render: String = content
}

abstract class HtmlTagGenerator extends Generator {
    protected val allowedTags: Set[String]
    protected val allowedAttributes: Map[String, List[String]]

    def generate(text: String): Option[ParseResult] = {
        val scanner = Scanner(text)

        if (!scanner.reads('<')) return None
        scanner.skip(1)

        if (scanner.reads("!--")) {
            if (scanner.find("-->")) {
                val endIndex = scanner.position + 3
                return Some((HtmlTag(text.substring(0, endIndex)), endIndex))
            } else return None
        }

        if (scanner.reads('/')) scanner.skip(1)

        val name = scanner.extractIdentifier
        if (name.isEmpty) return None
        else if (!allowedTags.contains(name)) return None

        while (!scanner.atEnd) {
            scanner.skipWhitespace()

            if (scanner.reads("/>"))
                return Some((HtmlTag(text.substring(0, scanner.position + 2)), scanner.position + 2))
            if (scanner.reads('>'))
                return Some((HtmlTag(text.substring(0, scanner.position + 1)), scanner.position + 1))

            val key = scanner.extractIdentifier
            if (!allowedAttributes(name).contains(key)) return None

            if (scanner.reads('=')) {
                scanner.skip(1)

                if (!scanner.find('>') || !scanner.find(' ')) return None
            }
        }

        None
    }
}

object DefaultHtmlTagGenerator extends HtmlTagGenerator {
    override val allowedTags = Set(
        "b",
        "blockquote",
        "code",
        "dd",
        "dt",
        "dl",
        "del",
        "em",
        "h1",
        "h2",
        "h3",
        "h4",
        "h5",
        "h6",
        "i",
        "li",
        "ol",
        "ul",
        "p",
        "pre",
        "s",
        "sub",
        "sup",
        "strong",
        "strike",
        "img",
        "a"
    )
    override val allowedAttributes = Map(
        "a" -> List("href", "title", "class"),
        "img" -> List("src", "width", "height", "alt", "title", "class"),
        "div" -> List("class")
    )
}