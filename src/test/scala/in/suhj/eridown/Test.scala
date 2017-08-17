package in.suhj.eridown

import org.scalatest.FunSuite

class Test extends FunSuite {
    test("Heading 1") {
        assert(Parser.render("# Heading") == "<h1>Heading</h1>")
    }

    test("Blockquote 1") {
        assert(Parser.render("> Blockquote") == "<blockquote>Blockquote</blockquote>")
    }

    test("Code 1") {
        assert(Parser.render(
            """
              |```scala
              |println("Hello, world!")
              |```
            """.stripMargin.trim) == """<pre class="lang-scala"><code>println("Hello, world!")</code></pre>""".trim.replaceAll("(\\s|\\r|\\n){2,}", ""))
    }

    test("List 1") {
        assert(Parser.render(
            """
              |* Eriri
              | * is
              |  1. so
              | * cute
              |* and
              |* lovely
            """.stripMargin.trim
        ) === "<ul><li>Eriri</li><ul><li>is</li><ul><li>so</li></ul><li>cute</li></ul><li>and</li><li>lovely</li></ul>")
    }

    test("Table 1") {
        assert(Parser.render(
            """
              |^ Left  |  Center  ||
              |^ ::: |   Right | Left |
            """.stripMargin
        ) === "<table><tr><th rowspan=\"2\">Left</th><td colspan=\"2\" class=\"center\">Center</td></tr><tr><td class=\"right\">Right</td><td>Left</td></tr></table>")
    }
}
