package in.suhj.eridown.core

import scala.collection.mutable.ListBuffer

abstract class Element {
    type IntegrateResult = (Element, Int)

    var length: Int = 0
    var indent: Int = 0
    var rawText: String = ""

    def render: String
    def integrate(targets: List[Element]): IntegrateResult = (this, 1)
}