package in.suhj.eridown.core

import in.suhj.eridown.elements.block.{Blank, ParagraphGenerator}
import in.suhj.eridown.elements.inline.{Text, TextGenerator}
import in.suhj.eridown.option.Option._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class Range(start: Int, end: Int)

abstract class Transformer {
    type ElementRange = (Element, Range)

    protected val generators: List[Generator]
    protected val fillGenerator: Generator
    protected def skipToNext(scanner: Scanner)

    final def transform(text: String): String = parse(text).map(_.render).mkString
    final def parse(text: String): List[Element] = {
        def fill(text: String) = (fillGenerator.create(text): @unchecked) match {
            case Some(result) => result
        }

        if (generators.isEmpty) return List(fill(text))

        val elementRanges = new ListBuffer[ElementRange]
        val scanner = Scanner(text)

        @tailrec
        def create(generators: List[Generator]): Option[ElementRange] =
            if (generators.isEmpty) None
            else generators.head.create(scanner.ahead) match {
                case Some(elem) =>
                    Some(elem -> Range(scanner.position, scanner.position + elem.length))
                case None => create(generators.tail)
            }

        while (!scanner.atEnd) {
            create(generators) match {
                case Some(elementRange) => {
                    elementRanges += elementRange
                    scanner.position = elementRange._2.end
                }
                case None => skipToNext(scanner)
            }
        }

        val elements = new ListBuffer[Element]
        // For the sake of the last plain text to be processed
        elementRanges += Blank() -> Range(text.length, text.length)

        for {
            i <- elementRanges.indices
            prev = i - 1
        } {
            val (element, Range(start, _)) = elementRanges(i)
            val formerEnd =
                if (prev < 0) 0
                else elementRanges(prev)._2.end

            val precedingTexts = text.substring(formerEnd, start).split("\n")

            for (text <- precedingTexts) {
                if (text.trim.nonEmpty) {
                    elements += fill(text)
                }
            }

            elements += element
        }
        elements.remove(elements.length - 1)

        val integrated = new ListBuffer[Element]

        while (elements.nonEmpty) {
            val item = elements.head
            val (newElem, consumedIndex) = item.integrate(elements.tail.toList)

            integrated += newElem
            elements.remove(0, consumedIndex)
        }

        integrated.toList
    }
}

object BlockTransformer extends Transformer {
    override val generators: List[Generator] = blocks
    override val fillGenerator: Generator = ParagraphGenerator
    override def skipToNext(scanner: Scanner) = scanner.skipToNextLine()
}

object InlineTransformer extends Transformer {
    override val generators: List[Generator] = inlines
    override val fillGenerator: Generator = TextGenerator
    override def skipToNext(scanner: Scanner) = scanner.skip(1)
}