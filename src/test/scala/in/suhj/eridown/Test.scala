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
            """.stripMargin.trim) == """<pre class="lang-scala"><code>println(&quot;Hello, world!&quot;)
                                      |</code></pre>""".stripMargin.trim.replaceAll("(\\s|\\r|\\n){2,}", ""))
    }

    test("Code 2") {
        assert(Parser.render(
            s"""
              |    console.log("Hello, world!");
              |${"\t"}var a = 1;
            """.stripMargin) == """<pre><code>console.log(&quot;Hello, world!&quot;);
                                                 |var a = 1;</code></pre>""".stripMargin)
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
        ) === "<ul><li>Eriri</li><ul><li>is</li><ol><li>so</li></ol><li>cute</li></ul><li>and</li><li>lovely</li></ul>")
    }

    test("Table 1") {
        assert(Parser.render(
            """
              |^ Left  |  Center  ||
              |^ ::: |   Right | Left |
            """.stripMargin
        ) === "<table><tr><th rowspan=\"2\">Left</th><td colspan=\"2\" class=\"center\">Center</td></tr><tr><td class=\"right\">Right</td><td>Left</td></tr></table>")
    }

    test("Definition List 1") {
        assert(Parser.render(
            """
              |; Term : Definition
              |; Term
              |: Definition
            """.stripMargin.trim
        ) == "<dl><dt>Term </dt><dd>Definition</dd><dt>Term</dt><dd>Definition</dd></dl>")
    }

    test("No Format 1") {
        assert(Parser.render(
            """
              |%%%
              |* We do nothing
              |
              |^ To the | markup. |
              |%%%
            """.stripMargin.trim
        ) == "\n* We do nothing\n\n^ To the | markup. |\n")
    }

    test("Paragraph 1") {
        assert(Parser.render(
            """
              |Hello, eridown!
            """.stripMargin.trim
        ) == "<p>Hello, eridown!</p>")
    }

    test("Paragraph 2") {
        assert(Parser.render(
            """
              |Hello!
              |
              |Eridown!
            """.stripMargin.trim
        ) == "<p>Hello!</p><p>Eridown!</p>")
    }

    test("Bold 1") {
        assert(Parser.render(
            """
              |Hello, **eridown**
            """.stripMargin.trim
        ) == "<p>Hello, <b>eridown</b></p>")
    }

    test("Bold 2") {
        assert(Parser.render(
            """
              |Hello, ***eri*down**
            """.stripMargin.trim
        ) == "<p>Hello, <b><em>eri</em>down</b></p>")
    }

    test("Emphasis 1") {
        assert(Parser.render(
            """
              |Hello, *eridown*
            """.stripMargin.trim
        ) == "<p>Hello, <em>eridown</em></p>")
    }

    test("Link 1") {
        assert(Parser.render(
            """
              |[**Eri**down](link)
            """.stripMargin.trim
        ) == """<p><a href="link"><b>Eri</b>down</a></p>""")
    }

    test("Strike 1") {
        assert(Parser.render(
            """
              |Hello, **eri~~*down*~~ up**
            """.stripMargin.trim
        ) == "<p>Hello, <b>eri<del><em>down</em></del> up</b></p>")
    }

    test("Image 1") {
        assert(Parser.render(
            """
              |![**Eri**down](link)
            """.stripMargin.trim
        ) == """<p><img src="link" alt="**Eri**down"></p>""")
    }

    test("No Format Inline 1") {
        assert(Parser.render(
            """
              |Hello, **%%eri~~*down*~~%% up**
            """.stripMargin.trim
        ) == "<p>Hello, <b>eri~~*down*~~ up</b></p>")
    }

    test("Html 1") {
        assert(Parser.render(
            """
              |<b>Hello, <em>eridown</em></b>
            """.stripMargin.trim
        ) == "<p><b>Hello, <em>eridown</em></b></p>")
    }

    test("Html 2") {
        assert(Parser.render(
            """
              |<script>alert(0);</script>
            """.stripMargin.trim
        ) == "<p>&lt;script&gt;alert(0);&lt;/script&gt;</p>")
    }
}
