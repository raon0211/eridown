package in.suhj.eridown

import org.scalatest.FunSuite

class LeafBlockTest extends FunSuite {
    test("***") {
        assert(Parser.render("***") == "<hr />")
    }

    test("---") {
        assert(Parser.render("---") == "<hr />")
    }

    test("___") {
        assert(Parser.render("___") == "<hr />")
    }
}
