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

        test("   > > 1.  one\n>>\n>>     two") {
            assert(Parser.render("   > > 1.  one\n>>\n>>     two") == "<blockquote><blockquote><ol><li><p>one</p><p>two</p></li></ol></blockquote></blockquote>")
        }

        test(">>- one\n>>\n  >  > two") {
            assert(Parser.render(">>- one\n>>\n  >  > two") == "<blockquote><blockquote><ul><li>one</li></ul><p>two</p></blockquote></blockquote>")
        }

        test("-one\n\n2.two") {
            assert(Parser.render("-one\n\n2.two") == "<p>-one</p><p>2.two</p>")
        }

        test("- foo\n\n\n  bar") {
            assert(Parser.render("- foo\n\n\n  bar") == "<ul><li><p>foo</p><p>bar</p></li></ul>")
        }

        test("1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam") {
            assert(Parser.render("1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam") ==
                "<ol><li><p>foo</p><pre><code>bar</code></pre><p>baz</p><blockquote><p>bam</p></blockquote></li></ol>")
        }

        test("- Foo\n\n      bar\n\n\n      baz") {
            assert(Parser.render("- Foo\n\n      bar\n\n\n      baz") ==
                "<ul><li><p>Foo</p><pre><code>bar\n\n\nbaz</code></pre></li></ul>")
        }

        test("123456789. ok") {
            assert(Parser.render("123456789. ok") ==
                "<ol start=\"123456789\"><li>ok</li></ol>")
        }

        test("1234567890. not ok") {
            assert(Parser.render("1234567890. not ok") ==
                "<p>1234567890. not ok</p>")
        }

        test("0. ok") {
            assert(Parser.render("0. ok") == "<ol start=\"0\"><li>ok</li></ol>")
        }

        test("003. ok") {
            assert(Parser.render("003. ok") == "<ol start=\"3\"><li>ok</li></ol>")
        }

        test("-1. not ok") {
            assert(Parser.render("-1. not ok") == "<p>-1. not ok</p>")
        }

        test("- foo\n\n      bar") {
            assert(Parser.render("- foo\n\n      bar") == "<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>")
        }

        test("  10.  foo\n\n           bar") {
            assert(Parser.render("  10.  foo\n\n           bar") == "<ol start=\"10\"><li><p>foo</p><pre><code>bar</code></pre></li></ol>")
        }

        test("    indented code\n\nparagraph\n\n    more code") {
            assert(Parser.render("    indented code\n\nparagraph\n\n    more code") == "<pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre>")
        }

        test("1.      indented code\n\n   paragraph\n\n       more code") {
            assert(Parser.render("1.      indented code\n\n   paragraph\n\n       more code") ==
                "<ol><li><pre><code> indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>")
        }

        test("   foo\n\nbar") {
            assert(Parser.render("   foo\n\nbar") == "<p>foo</p><p>bar</p>")
        }

        test("-    foo\n\n  bar") {
            assert(Parser.render("-    foo\n\n  bar") == "<ul><li>foo</li></ul><p>bar</p>")
        }

        test("-  foo\n\n   bar") {
            assert(Parser.render("-  foo\n\n   bar") == "<ul><li><p>foo</p><p>bar</p></li></ul>")
        }

    test("-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz") {
        assert(Parser.render("-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz") ==
            "<ul><li>foo</li><li><pre><code>bar</code></pre></li><li><pre><code>baz</code></pre></li></ul>")
    }

    test("-   \n  foo") {
        assert(Parser.render("-   \n  foo") == "<ul><li>foo</li></ul>")
    }

    test("-\n\n  foo") {
        assert(Parser.render("-\n\n  foo") == "<ul><li></li></ul><p>foo</p>")
    }

    test("- foo\n-\n- bar") {
        assert(Parser.render("- foo\n-\n- bar") == "<ul><li>foo</li><li></li><li>bar</li></ul>")
    }

    test("- foo\n-   \n- bar") {
        assert(Parser.render("- foo\n-   \n- bar") == "<ul><li>foo</li><li></li><li>bar</li></ul>")
    }

    test("1. foo\n2.\n3. bar") {
        assert(Parser.render("1. foo\n2.\n3. bar") == "<ol><li>foo</li><li></li><li>bar</li></ol>")
    }

    test("*") {
        assert(Parser.render("*") == "<ul><li></li></ul>")
    }

    test("foo\n*\n\nfoo\n1.") {
        assert(Parser.render("foo\n*\n\nfoo\n1.") == "<p>foo\n*</p><p>foo\n1.</p>")
    }

    test(" 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.") {
        assert(Parser.render(" 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.")
            == "<ol><li><p>A paragraph\nwith two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>")
    }

    test("  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.") {
        assert(Parser.render("  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.")
            == "<ol><li><p>A paragraph\nwith two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>")
    }

    test("   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.") {
        assert(Parser.render("   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.")
            == "<ol><li><p>A paragraph\nwith two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>")
    }

    test("    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.") {
        assert(Parser.render("    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.")
            == """<pre><code>1.  A paragraph
                 |    with two lines.
                 |
                 |        indented code
                 |
                 |    &gt; A block quote.</code></pre>""".stripMargin)
    }

    test("  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.") {
        assert(Parser.render("  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.")
            ==
            """<ol><li><p>A paragraph
              |with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>""".stripMargin)
    }

    test("  1.  A paragraph\n    with two lines.") {
        assert(Parser.render("  1.  A paragraph\n    with two lines.")
            == "<ol><li>A paragraph\nwith two lines.</li></ol>")
    }

    test("> 1. > Blockquote\ncontinued here.") {
        assert(Parser.render("> 1. > Blockquote\ncontinued here.")
            == "<blockquote><ol><li><blockquote><p>Blockquote\ncontinued here.</p></blockquote></li></ol></blockquote>")
    }
}
