package in.suhj.eridown

import org.scalatest.FunSuite

class ListTest extends FunSuite {
    test("A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.") {
        assert(Parser.render("""A paragraph
                               |with two lines.
                               |
                               |    indented code
                               |
                               |> A block quote.""".stripMargin.trim) ==
            """<p>A paragraph
              |with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote>""".stripMargin.trim)
    }

    test("1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.") {
        assert(Parser.render("""1.  A paragraph
                               |    with two lines.
                               |
                               |        indented code
                               |
                               |    > A block quote.""".stripMargin.trim) ==
            """<ol><li><p>A paragraph
              |with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>""".stripMargin.trim)
    }

    test("- one\n\n two") {
        assert(Parser.render("- one\n\n two") == "<ul><li>one</li></ul><p>two</p>")
    }

    test("- one\n\n  two") {
        assert(Parser.render("- one\n\n  two") == "<ul><li><p>one</p><p>two</p></li></ul>")
    }

    test(" -    one\n\n     two") {
        assert(Parser.render(" -    one\n\n     two") == "<ul><li>one</li></ul><pre><code> two</code></pre>")
    }

    test(" -    one\n\n      two") {
        assert(Parser.render(" -    one\n\n      two") == "<ul><li><p>one</p><p>two</p></li></ul>")
    }
}
