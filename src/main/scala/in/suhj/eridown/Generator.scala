package in.suhj.eridown

import in.suhj.eridown.elements.block._
import in.suhj.eridown.elements.inline._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

abstract class ParseResult
case class Valid(element: Element, rawLength: Int) extends ParseResult
case class Invalid() extends ParseResult

case class Range(start: Int, end: Int)
case class ElementRange(element: Element, range: Range)

abstract class Generator {
    protected def generators: List[Generator]
    protected def fillGenerator: Generator
    protected def skipToNext(scanner: Scanner)

    protected def generate(text: String): ParseResult

    protected final def getChildrenData(generator: Generator, text: String): List[Valid] = {
        @tailrec
        def readChild(items: List[Valid], text: String): List[Valid] = {
            generator.generate(text) match {
                case Invalid() => items.reverse
                case valid @ Valid(_, length) => {
                    readChild(valid :: items, text.substring(length))
                }
            }
        }
        readChild(Nil, text)
    }

    protected final def transform(text: String): String = {
        def fillRender(text: String) = fillGenerator.generate(text).asInstanceOf[Valid].element.render

        if (generators.isEmpty) return fillRender(text)

        val elements = new ListBuffer[ElementRange]()
        val scanner = Scanner(text)

        while (!scanner.atEnd) {
            @tailrec
            def generate(generators: List[Generator]): ElementRange =
                generators.head.generate(scanner.ahead) match {
                    case Valid(elem, length) => ElementRange(elem, Range(scanner.position, scanner.position + length))
                    case Invalid() => generate(generators.tail)
                }

            try {
                val result = generate(generators)
                elements += result

                scanner.position = result.range.end
            } catch {
                case _: NoSuchElementException => skipToNext(scanner)
            }

            scanner.skipLineEnd()
        }

        if (elements.isEmpty) return fillRender(text)

        var renders = new ListBuffer[String]
        elements += ElementRange(Text(""), Range(text.length, text.length)) // For the sake of the last plain text to be processed

        for {
            i <- elements.indices
            prev = i - 1
        } {
            val ElementRange(element, Range(start, _)) = elements(i)
            val formerEnd =
                if (prev < 0) 0
                else elements(prev).range.end

            val precedingText = text.substring(formerEnd, start)

            if (!precedingText.trim.isEmpty) {
                renders += fillRender(precedingText)
            }

            renders += element.render
        }

        renders.mkString
    }
}

abstract class MarkdownGenerator extends Generator {
    protected def blocks: List[MarkdownGenerator] = List(
        HeadingGenerator,
        BlockquoteGenerator,
        CodeGenerator,
        ListGenerator,
        TableGenerator,
        DefinitionListGenerator,
        NoFormatGenerator
    )
    protected def inlines: List[MarkdownGenerator] = List(
        BoldGenerator,
        EmphasisGenerator,
        StrikeGenerator,
        CodeInlineGenerator,
        LinkGenerator,
        ImageGenerator,
        NoFormatInlineGenerator
    )

    val isBlock: Boolean
    override def generators: List[Generator] = inlines
    override def fillGenerator: Generator = TextGenerator
    override def skipToNext(scanner: Scanner) = scanner.skip(1)
}