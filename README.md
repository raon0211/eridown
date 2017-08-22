Eridown
=========

> Scalable, fast, and accurate. Completely customizable.

```scala
import in.suhj.eridown.Parser

println(Parser.render("Hello, *eridown*!"))
// -> <p>Hello, <em>eridown</em>!</p>
```

Eridown is a project that aims for a *scalable*(also *extensible*), *fast*, and *accurate* Markdown parser written in the powerful Scala language. Also, it provides an extension of Markdown, named *Eridown*, which has many convenient syntaxes such as tables, definition lists, and no-format elements. It automatically sanitizes HTML, too!

# What's so special?

## Scalability

The main aim of this project is to support scalability in markdown parsers. Many contemporary markdown parsers cannot be *extended* or *downsized*, i.e. everyone had to use all the [CommonMark specs](http://spec.commonmark.org/), GFM specs, etc. 

However Eridown changes this paradigm. Let's take an example of extending Eridown. By only writing a markdown *element class* and *generator object*, you can easily extend the markdown syntax.

Suppose that we want to make our own `Capitalized` inline element, which generates a capitalized `TEXT` from `+text+`. If the input is like:

```
Eridown is an +awesome+ markdown parser!
```

the output would be:

> Eridown is an AWESOME markdown parser!

We write the code as following:

```scala
import in.suhj.eridown._

case class Capitalized(text: String) extends Element {
    def render = s"<span style=\"text-transform: uppercase;\">$text</span>"
}

object CapitalizedGenerator extends Generator {
    def generate(text: String): ParseResult = {
        if (text(0) != '+') return Invalid()

        val index = text.indexOf('+', 1)
        if (index < 0) return Invalid()

        val content = text.substring(1, index)
        if (content.isEmpty) return Invalid()

        val element = Capitalized(transform(content))
        Valid(element, index + 1)
    }
}
```

The class `Capitalized` is our element class, which is used to store data and render that data afterwards. The generator object, in this case `CapitalizedGenerator`, is what matches the pattern from the document and 'generates' the element object from it.

The `generate` method of generator object is provided the text which starts from the position where the element is expected to begin. It is given any substring of the input: it may be something like `+awesome+ markdown parser!`(which will succeed in matching the pattern) or `kdown parser!`(which will fail).

Inside `generate`, we first check if the given text matches our start condition. If the text doesn't start with `+`, it is not our pattern. Whenever the text doesn't match our pattern, we must return the `Invalid` result. So we return the `Invalid` object.

Also, if the closing `+` doesn't exist, it doesn't match our pattern. Hence we also return the `Invalid` result then. Afterwards, we extract the content surronded by two `+`s. 

We now produce our final `Capitalized` element object. The method `transform` parses nested elements and return the rendered text of it.

Finally, we return the `Valid` object, which has as the fields the element and `rawLength: Int`, indicating the length of our element at the raw text. The valid `rawLength` has to be returned in order to parse the next element following correctly.

Easy, huh? The work left to be done is only registering the generator object by specifying the parameters `blocks` and `inlines`:

```scala
import in.suhj.eridown.Parser
import in.suhj.eridown.option.Constants._

val myInlines = eridownInlines ::: List(CapitalizedGenerator)
println(Parser.render("Some +text+", eridownBlocks, myInlines))
```

Check out our [scaling guide](#scaling-guide) which provides more information about easy scaling.

## Speed

Another aim of this project is speed. By introducing the *scanner*, we could increase the speed of our parser. Detailed benchmarks will follow soon.

## Accuracy

Every parser must not abandon accuracy. Eridown will produce accurate results according to the CommonMark specifications, plus some convenient additional syntaxes. Of course, you could also use only CommonMark specs to parse your document, using `Parser.commonMarkParse(text: String)`. 

Were you to find any errors or mistakes in Eridown, feel free to open an issue at our issue tracker.

# Scaling guide

You have seen basic ways to extend the Eridown parser in the [What's so special?] -> [[Scalability](#scalability)] section. Now we explain this in detail.

## The *element class*

The element class is what stores basic information about the data and renders that data provided when needed. Basically all elements classes have to extend the `Element` class, which requires you to implement the `render` method:

```scala
abstract class Element {
    def render: String
}
```

A simple example of the element would be the `Capitalized` element mentioned [above](#scalability):

```scala
case class Capitalized(text: String) extends Element {
    def render = s"<span style=\"font-size: 1.25em\">$text</span>"
}
```

## The *generator object*

The generator object is what matches the pattern from the document and generates the element from it. Every generator object extends the abstract `Generator` class. By extending the `Generator` class, you *have to* define the method:

```scala
def generate(text: String): ParseResult
```

It tries to parse the `text` and returns the result of it. The `ParseResult` class is abstract and has two subclasses:

```scala
abstract class ParseResult
case class Valid(element: Element, rawLength: Int) extends ParseResult
case class Invalid() extends ParseResult
```

Simply you return `Valid(element, rawLength)` when you succeed in parsing, and `Invalid()` when you fail. `rawLength` indicates the length which our element occupies in the text.

> ***Note***: You have to return the proper `rawLength` when parsing, otherwise the parser will break. 

A simple example of the generator object is written [above](#scalability). Have a look at the code and explanation.

## Introducing the *scanner*

When you write your generator object, it is hard to calculate all the indexes and length of the string. For people's convenience, we have implemented the `Scanner` class, which makes you deal with strings efficiently. 

We can rewrite the code of the `Capitalized` element's generator object as following, when using the `Scanner` class:

```scala
import in.suhj.eridown.core.Scanner

object CapitalizedGenerator extends Generator {
    def generate(text: String): ParseResult = {
        val scanner = Scanner(text)

        if (!scanner.reads('+')) return Invalid()
        scanner.skip(1)

        scanner.mark()
        if (!scanner.find('+')) return Invalid()

        val content = scanner.extract
        if (content.isEmpty) return Invalid()

        Valid(Capitalized(transform(content)), scanner.position + 1)
    }
}
```

Detailed explanation is as follows:

First, we create an instance of the `Scanner` class. When the `Scanner` class is instantiated with `Scanner(text: String)`, the internal state of the object is made as(let's assume that the given `text` is `+awesome+ markdown parser!`)

```
buffer:      +awesome+ markdown parser!
position:    ^                           [Value: 0]
currentChar: +
```

The `position` field indicates where the `Scanner` object is scanning.

Second, The `scanner`'s `reads(char: Char)` method checks if the given `char` matches the current position's character, So it checks if `'+' == '+'`: true. Since it is true, the scanner object skips forward by 1 character(`scanner.skip(1)`):

```
buffer:      +awesome+ markdown parser!
position:     ^                          [Value: 1]
currentChar: a
```

Third, we call the `Scanner`'s `mark` method, which places a mark where indicating where the content starts.

```
buffer:      +awesome+ markdown parser!
position:     ^                          [Value: 1]
mark:         [
currentChar: a
```

Fourth, the `Scanner`'s `find(char: Char)` method finds the index of the next available character that is ahead of the current position and matches `char`. If the index is found, it moves the `position` to that index.

```
buffer:      +awesome+ markdown parser!
position:            ^                   [Value: 1]
mark:         [
currentChar: +
```

Finally, the `extract` method extracts the substring o f the buffer starting from the marked position(inclusive) and ending at the current position(exclusive). So in this case `scanner.extract == "awesome"`. So based on this result, we create the element object, as we did earlier.

As you can see, using `Scanner` is less error-prone and it provides many convenient methods for you to parse the code. We do not require you to use `Scanner`: it is on your choice. But using it will truly make your code more understandable.

Here is the interface of the `Scanner` class:

**The `Scanner` companion object's methods**

`Scanner` builder methods
* `Scanner.apply(buffer: String)`: Creates a new object of `Scanner`, which scans the whole `buffer`.
* `Scanner.apply(buffer: String, start: Int, end: Int)`: Creates a new object of `Scanner`, only scanning from the index start(inclusive) to end(exclusive).
* `Scanner.apply(buffer: String, start: Int)`: Creates a new object of `Scanner`, scanning from the index start(inclusive) to the end of the `buffer`.

Utility methods
* `Scanner.isLineEnd(char: Char)`: Check if `char` equals `\n`, `\r`, `\0`.
* `Scanner.isWhitespace(char: Char)`: Check if `char` equals space or `\t`.

**The `Scanner` class's interface**

*Fields:*

* `val buffer: String`: The current buffer of the scanner.
* `var position: Int`: The current position of the scanner.
* `var start: Int`: The position for the scanner to start scanning(inclusive).
* `var end: Int`: The position for the scanner to end scanning(exclusive).

*Methods:*

Returning the `buffer` or, a substring or character of it
* `def text: String`: returns the string to be scanned. If `start` and `end` is specified, returns `buffer.substring(start, end)`.
* `def ahead: String`: returns the string not yet scanned, i.e. the substring from `position`(inclusive) to end of the buffer.
* `def charAt(offset: Int)`: returns the character which is `offset` ahead from the current `position`. If the that position is not valid(outside `[start, end)`), return `\0`.
* `def currentChar: Char`: equals `charAt(0)`.

Predicates of `position` or `currentChar`
* `def atStart: Boolean`: returns if `position` is less than or equal to `start`.
* `def atEnd: Boolean`: returns if `position` is greater than or equal to `end`.
* `def atLineEnd: Boolean`: returns `Scanner.isLineEnd(currentChar)`.
* `def atWhitespace: Boolean`: returns `Scanner.isWhitespace(currentChar)`.

Pattern-matching helpers
* `def reads(char: Char): Boolean`: returns if `currentChar` equals `char`.
* `def reads(str: String): Boolean`: returns if `ahead` starts with `str`.
* `def reads(chars: Seq[Char]): Boolean`: returns if `currentChar` equals any character of the `chars`.

Content extractors
* `def mark(): Unit`: set `marker` to `position`.
* `def extract: String`: returns the string between `marker` and `position`. It doesn't matter which of the two is greater.
* `def extractIdentifier: String`: returns an *identifier*(starts with a letter or underscore, continued with a letter, underscore, or digit) starting from `position`.

Methods to move the `position` front and back
* `def skip(offset: Int): Unit`: add `offset` to `position`
* `def skipWhitespace(): Unit`: skip whitespaces starting from `position`
* `def skipLineEnd(): Unit`: skip line ends starting from `position`
* `def skipToLineEnd(): Unit`: skip characters until it finds a line end. If no line end is found, move the `position` to `end`.
* `def skipToNextLine(): Unit`: `skipToLineEnd()` then `skipLineEnd()`

Finding the offset of a given character
* `def seek(char: Char): Int`: returns `seek(String.valueOf(char))`
* `def seek(str: String): Int`: find the offset between `position` and next position of `str` in `ahead`. If `char` doesn't exist in `ahead`, -1.
* `def seekAny(chars: Seq[Char]): Int`: returns the least positive offset from `position` to the character from `chars`.

Moving to the next given character or string
* `def find(char: Char): Boolean`: returns `find(String.valueOf(char))`
* `def find(str: String)`: Check if `str` is in `ahead`. If true, move `position` there and return `true`. else return `false`.