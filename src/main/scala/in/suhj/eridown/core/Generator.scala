package in.suhj.eridown.core

import in.suhj.eridown.option.Option._
import in.suhj.eridown.elements.inline._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class ParseResult(element: Element, rawLength: Int)

case class Range(start: Int, end: Int)
case class ElementRange(element: Element, range: Range)

abstract class Generator {
    protected def generators: List[Generator] = inlines
    protected def fillGenerator: Generator = TextGenerator
    protected def skipToNext(scanner: Scanner) = scanner.skip(1)

    protected def generate(text: String): Option[ParseResult]

    protected final def getChildrenData(generator: Generator, text: String): List[ParseResult] = {
        @tailrec
        def readChild(items: List[ParseResult], text: String): List[ParseResult] = {
            generator.generate(text) match {
                case None => items.reverse
                case Some(valid @ ParseResult(_, length)) => {
                    readChild(valid :: items, text.substring(length))
                }
            }
        }

        readChild(Nil, text)
    }

    protected final def transform(text: String): String = {
        def fillRender(text: String) = (fillGenerator.generate(text): @unchecked) match {
            case Some(result) => result.element.render
        }

        if (generators.isEmpty) return fillRender(text)

        val elements = new ListBuffer[ElementRange]()
        val scanner = Scanner(text)

        while (!scanner.atEnd) {
            @tailrec
            def generate(generators: List[Generator]): Option[ElementRange] =
                if (generators.isEmpty) None
                else generators.head.generate(scanner.ahead) match {
                    case Some(ParseResult(elem, length)) =>
                        Some(ElementRange(elem, Range(scanner.position, scanner.position + length)))
                    case None => generate(generators.tail)
                }

            generate(generators) match {
                case Some(elementRange) => {
                    elements += elementRange
                    scanner.position = elementRange.range.end
                }
                case None => skipToNext(scanner)
            }
        }

        if (elements.isEmpty) return fillRender(text)

        var renders = new ListBuffer[String]
        // For the sake of the last plain text to be processed
        elements += ElementRange(Text(""), Range(text.length, text.length))

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