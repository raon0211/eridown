package in.suhj.eridown

import in.suhj.eridown.elements._
import in.suhj.eridown.elements.block._
import in.suhj.eridown.option.Option
import in.suhj.eridown.option.Constants._
import in.suhj.eridown.core.Generator

object Parser {
    def render(text: String): String = render(text, eridownBlocks, eridownInlines)
    def render(text: String, blocks: List[Generator], inlines: List[Generator]) = {
        Option.blocks = blocks ::: List(BlankGenerator)
        Option.inlines = inlines
        DocumentGenerator.parse(text.replace("\r\n", "\n")).render
    }
}