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
}
