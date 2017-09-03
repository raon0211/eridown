package in.suhj.eridown.core

import in.suhj.eridown.elements.block.Blank
import in.suhj.eridown.option.Option._
import in.suhj.eridown.elements.inline._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

abstract class Generator {
    type GenerateResult = (Element, Int)

    def create(text: String): Option[Element] = {
        var index = 0
        var indent = 0
        if (text.nonEmpty) {
            while (index < text.length && Scanner.isWhitespace(text(index))) {
                (text(index): @unchecked) match {
                    case ' ' => indent += 1
                    case '\t' => indent += 4
                }
                index += 1
            }
        }

        generate(text) match {
            case Some((elem, length)) => {
                elem.indent = indent
                elem.length = length
                elem.rawText = text.substring(0, length).stripLineEnd
                Some(elem)
            }
            case None => None
        }
    }
    def generate(text: String): Option[GenerateResult]

    protected final def getChildrenData(generator: Generator, text: String): List[Element] = {
        @tailrec
        def readChild(items: List[Element], text: String): List[Element] = {
            generator.create(text) match {
                case None => items.reverse
                case Some(elem) => {
                    readChild(elem :: items, text.substring(elem.length))
                }
            }
        }

        readChild(Nil, text)
    }
}