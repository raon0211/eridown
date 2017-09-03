package in.suhj.eridown.elements

import in.suhj.eridown.core._
import in.suhj.eridown.option.Option._
import in.suhj.eridown.elements.block._

case class Document(val content: String) extends Element {
    def render = content
}

object DocumentGenerator extends Generator {
    def generate(content: String): Option[GenerateResult] =
        Some(parse(content), 1)
    def parse(content: String): Document =
        Document(BlockTransformer.transform(content))
}