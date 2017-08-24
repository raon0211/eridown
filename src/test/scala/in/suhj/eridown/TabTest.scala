package in.suhj.eridown

import org.scalatest.FunSuite

class TabTest extends FunSuite {
    test("→foo→baz→→bim 1") {
        assert(Parser.render("\tfoo\tbaz\t\tbim") == "<pre><code>foo\tbaz\t\tbim</code></pre>")
    }

    test("→foo→baz→→bim 2") {
        assert(Parser.render("  \tfoo\tbaz\t\tbim") == "<pre><code>foo\tbaz\t\tbim</code></pre>")
    }

    test("    a→a\n    ὐ→a") {
        assert(Parser.render("    a→a\n    ὐ→a") == "<pre><code>a→a\nὐ→a</code></pre>")
    }
}
