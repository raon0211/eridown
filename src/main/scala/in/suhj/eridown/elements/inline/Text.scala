package in.suhj.eridown.elements.inline

import in.suhj.eridown._
import in.suhj.eridown.core.{Element, Generator}

import scala.collection.mutable.ListBuffer
import scala.xml.Utility.escape

case class Text(content: String) extends Element {
    def render = escape(content)

    override def integrate(targets: List[Element]) = {
        var canIntegrate = true
        var index = 0

        var content = new ListBuffer[String] += this.content

        while (canIntegrate && index < targets.length) {
            targets(index) match {
                case text: Text => {
                    content += text.content
                    index += 1
                }
                case _ => canIntegrate = false
            }
        }

        (Text(content.mkString("\n")), index + 1)
    }
}

object TextGenerator extends Generator {
    def generate(text: String) = Some((Text(text), text.length))
}