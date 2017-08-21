package in.suhj.eridown.option

import in.suhj.eridown.core.Generator
import in.suhj.eridown.option.Constants._

private[in] object Option {
    var blocks: List[Generator] = eridownBlocks
    var inlines: List[Generator] = eridownInlines
}