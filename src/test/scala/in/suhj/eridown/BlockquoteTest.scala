package in.suhj.eridown

import org.scalatest.FunSuite

class BlockquoteTest extends FunSuite {
    test("> # Foo\n> bar\n> baz") {
        assert(Parser.render("""> # Foo
                               |> bar
                               |> baz""".stripMargin.trim) ==
            """<blockquote><h1>Foo</h1><p>bar
              |baz</p></blockquote>""".stripMargin.trim)
    }

    test("># Foo\n>bar\n> baz") {
        assert(Parser.render(
            """
              |># Foo
              |>bar
              |> baz
            """.stripMargin.trim) ==
        "<blockquote><h1>Foo</h1><p>bar\nbaz</p></blockquote>")
    }

    test("   > # Foo\n   > bar\n > baz") {
        assert(Parser.render(
            """
              |   > # Foo
              |   > bar
              | > baz
            """.stripMargin.trim) ==
            "<blockquote><h1>Foo</h1><p>bar\nbaz</p></blockquote>"
        )
    }

    test("    > # Foo\n    > bar\n    > baz") {
        assert(Parser.render(
            """
              |    > # Foo
              |    > bar
              |    > baz
            """.stripMargin) ==
            "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz</code></pre>"
        )
    }
}