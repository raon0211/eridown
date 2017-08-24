package in.suhj.eridown.option

import in.suhj.eridown.core.Generator
import in.suhj.eridown.elements.block._
import in.suhj.eridown.elements.inline._

object Constants {
    val eridownBlocks: List[Generator] = List(
        CodeGenerator,
        ThematicBreakGenerator,
        HeadingGenerator,
        BlockquoteGenerator,
        ListGenerator,
        TableGenerator,
        DefinitionListGenerator,
        NoFormatGenerator
    )
    val eridownInlines: List[Generator] = List(
        BoldGenerator,
        EmphasisGenerator,
        StrikeGenerator,
        CodeInlineGenerator,
        LinkGenerator,
        ImageGenerator,
        DefaultHtmlTagGenerator,
        NoFormatInlineGenerator
    )
}
