package in.suhj.eridown

import in.suhj.eridown.Parser._
import org.scalatest.FunSuite

class LeafBlockTest extends FunSuite {
    test("***") {
        assert(render("***") == "<hr />")
    }

    test("---") {
        assert(render("---") == "<hr />")
    }

    test("___") {
        assert(render("___") == "<hr />")
    }

    test("+++") {
        assert(render("+++") == "<p>+++</p>")
    }

    test("===") {
        assert(render("===") == "<p>===</p>")
    }
    
    test("--\n**\n__") {
        assert(render("--\n**\n__") == "<p>--\n**\n__</p>")
    }

    test(" ***\n  ***\n   ***") {
        assert(render(" ***\n  ***\n   ***") == "<hr /><hr /><hr />")
    }

    test("    *** 2") {
        assert(render("    ***") == "<pre><code>***</code></pre>")
    }

    test("Foo\n    ***") {
        assert(render("Foo\n    ***") == "<p>Foo\n***</p>")
    }

    test("_____________________________________") {
        assert(render("_____________________________________") == "<hr />")
    }

    test(" - - -") {
        assert(render(" - - -") == "<hr />")
    }

    test(" **  * ** * ** * **") {
        assert(render(" **  * ** * ** * **") == "<hr />")
    }

    test("-     -      -      -") {
        assert(render("-     -      -      -") == "<hr />")
    }

    test("- - - -    ") {
        assert(render("- - - -    ") == "<hr />")
    }

    test("_ _ _ _ a\n\na------\n\n---a---") {
        assert(render("_ _ _ _ a\n\na------\n\n---a---") == "<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>")
    }

    test(" *-*") {
        assert(render(" *-*") == "<p><em>-</em></p>")
    }

    test("- foo\n***\n- bar") {
        assert(render("- foo\n***\n- bar") == "<ul><li>foo</li></ul><hr /><ul><li>bar</li></ul>")
    }

    test("Foo\n***\nbar") {
        assert(render("Foo\n***\nbar") == "<p>Foo</p><hr /><p>bar</p>")
    }

    test("Foo\n---\nbar") {
        assert(render("Foo\n---\nbar") == "<h2>Foo</h2><p>bar</p>")
    }

    test("* Foo\n* * *\n* Bar") {
        assert(render("* Foo\n* * *\n* Bar") == "<ul><li>Foo</li></ul><hr /><ul><li>Bar</li></ul>")
    }

    test("- Foo\n- * * *") {
        assert(render("- Foo\n- * * *") == "<ul><li>Foo</li><li><hr /></li></ul>")
    }
}
