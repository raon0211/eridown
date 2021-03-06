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

    test("> 1. > Blockquote\n> continued here.") {
        assert(Parser.render("> 1. > Blockquote\n> continued here.")
            == "<blockquote><ol><li><blockquote><p>Blockquote\ncontinued here.</p></blockquote></li></ol></blockquote>")
    }

    test("- foo\n  - bar\n    - baz\n      - boo") {
        assert(Parser.render("- foo\n  - bar\n    - baz\n      - boo")
            == "<ul><li>foo<ul><li>bar<ul><li>baz<ul><li>boo</li></ul></li></ul></li></ul></li></ul>")
    }

    test("- foo\n - bar\n  - baz\n   - boo") {
        assert(Parser.render("- foo\n - bar\n  - baz\n   - boo")
            == "<ul><li>foo</li><li>bar</li><li>baz</li><li>boo</li></ul>")
    }

    test("10) foo\n    - bar") {
        assert(Parser.render("10) foo\n    - bar")
            == "<ol start=\"10\"><li>foo<ul><li>bar</li></ul></li></ol>")
    }

    test("10) foo\n   - bar") {
        assert(Parser.render("10) foo\n   - bar")
            == "<ol start=\"10\"><li>foo</li></ol><ul><li>bar</li></ul>")
    }

    test("- - foo") {
        assert(Parser.render("- - foo")
            == "<ul><li><ul><li>foo</li></ul></li></ul>")
    }

    test("1. - 2. foo") {
        assert(Parser.render("1. - 2. foo")
            == "<ol><li><ul><li><ol start=\"2\"><li>foo</li></ol></li></ul></li></ol>")
    }

    test("- # Foo\n- Bar\n  ---\n  baz") {
        assert(Parser.render("- # Foo\n- Bar\n  ---\n  baz")
            == "<ul>\n<li>\n<h1>Foo</h1>\n</li>\n<li>\n<h2>Bar</h2>\nbaz</li>\n</ul>")
    }

    test("- foo\n- bar\n+ baz") {
        assert(Parser.render("- foo\n- bar\n+ baz")
            == "<ul><li>foo</li><li>bar</li></ul><ul><li>baz</li></ul>")
    }

    test("1. foo\n2. bar\n3) baz") {
        assert(Parser.render("1. foo\n2. bar\n3) baz")
            == "<ol><li>foo</li><li>bar</li></ol><ol start=\"3\"><li>baz</li></ol>")
    }

    test("Foo\n- bar\n- baz") {
        assert(Parser.render("Foo\n- bar\n- baz")
            == "<p>Foo</p><ul><li>bar</li><li>baz</li></ul>")
    }

    test("The number of windows in my house is\n14.  The number of doors is 6.") {
        assert(Parser.render("The number of windows in my house is\n14.  The number of doors is 6.")
            == "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>")
    }

    test("The number of windows in my house is\n1.  The number of doors is 6.") {
        assert(Parser.render("The number of windows in my house is\n1.  The number of doors is 6.")
            == "<p>The number of windows in my house is</p><ol><li>The number of doors is 6.</li></ol>")
    }

    test("- foo\n\n- bar\n\n\n- baz") {
        assert(Parser.render("- foo\n\n- bar\n\n\n- baz")
            == "<ul><li><p>foo</p></li><li><p>bar</p></li><li><p>baz</p></li></ul>")
    }

    test("- foo\n  - bar\n    - baz\n\n\n      bim") {
        assert(Parser.render("- foo\n  - bar\n    - baz\n\n\n      bim")
            == "<ul><li>foo<ul><li>bar<ul><li><p>baz</p><p>bim</p></li></ul></li></ul></li></ul>")
    }

    test("- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim") {
        assert(Parser.render("- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim")
            == "<ul><li>foo</li><li>bar</li></ul><!-- --><ul><li>baz</li><li>bim</li></ul>")
    }

    test("-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code") {
        assert(Parser.render("-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code")
            == "<ul><li><p>foo</p><p>notcode</p></li><li><p>foo</p></li></ul><!-- --><pre><code>code</code></pre>")
    }

    test("- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i") {
        assert(Parser.render("- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i")
            == "<ul><li>a</li><li>b</li><li>c</li><li>d</li><li>e</li><li>f</li><li>g</li><li>h</li><li>i</li></ul>")
    }

    test("1. a\n\n  2. b\n\n    3. c") {
        assert(Parser.render("1. a\n\n  2. b\n\n    3. c")
            == "<ol><li><p>a</p></li><li><p>b</p></li><li><p>c</p></li></ol>")
    }

    test("- a\n- b\n\n- c") {
        assert(Parser.render("- a\n- b\n\n- c")
            == "<ul><li><p>a</p></li><li><p>b</p></li><li><p>c</p></li></ul>")
    }

    test("* a\n*\n\n* c") {
        assert(Parser.render("* a\n*\n\n* c")
            == "<ul><li><p>a</p></li><li></li><li><p>c</p></li></ul>")
    }

    test("- a\n- b\n\n  c\n- d") {
        assert(Parser.render("- a\n- b\n\n  c\n- d")
            == "<ul><li><p>a</p></li><li><p>b</p><p>c</p></li><li><p>d</p></li></ul>")
    }

    test("- a\n- b\n\n  [ref]: /url\n- d") {
        assert(Parser.render("- a\n- b\n\n  [ref]: /url\n- d")
            == "<ul><li><p>a</p></li><li><p>b</p></li><li><p>d</p></li></ul>")
    }

    test("- a\n- ```\n  b\n\n\n  ```\n- c") {
        assert(Parser.render("- a\n- ```\n  b\n\n\n  ```\n- c")
            == "<ul><li>a</li><li><pre><code>b\n\n</code></pre></li><li>c</li></ul>")
    }

    test("- a\n  - b\n\n    c\n- d") {
        assert(Parser.render("- a\n  - b\n\n    c\n- d")
            == "<ul><li>a<ul><li><p>b</p><p>c</p></li></ul></li><li>d</li></ul>")
    }

    test("* a\n  > b\n  >\n* c") {
        assert(Parser.render("* a\n  > b\n  >\n* c")
            == "<ul><li>a<blockquote><p>b</p></blockquote></li><li>c</li></ul>")
    }

    test("- a\n  > b\n  ```\n  c\n  ```\n- d") {
        assert(Parser.render("- a\n  > b\n  ```\n  c\n  ```\n- d")
            == "<ul><li>a<blockquote><p>b</p></blockquote><pre><code>c</code></pre></li><li>d</li></ul>")
    }

    test("- a") {
        assert(Parser.render("- a")
            == "<ul><li>a</li></ul>")
    }

    test("- a\n  - b") {
        assert(Parser.render("- a\n  - b")
            == "<ul><li>a<ul><li>b</li></ul></li></ul>")
    }

    test("1. ```\n   foo\n   ```\n\n   bar") {
        assert(Parser.render("1. ```\n   foo\n   ```\n\n   bar")
            == "<ol><li><pre><code>foo</code></pre><p>bar</p></li></ol>")
    }

    test("* foo\n  * bar\n\n  baz") {
        assert(Parser.render("* foo\n  * bar\n\n  baz")
            == "<ul><li><p>foo</p><ul><li>bar</li></ul><p>baz</p></li></ul>")
    }

    test("- a\n  - b\n  - c\n\n- d\n  - e\n  - f") {
        assert(Parser.render("- a\n  - b\n  - c\n\n- d\n  - e\n  - f")
            == "<ul><li><p>a</p><ul><li>b</li><li>c</li></ul></li><li><p>d</p><ul><li>e</li><li>f</li></ul></li></ul>")
    }
}