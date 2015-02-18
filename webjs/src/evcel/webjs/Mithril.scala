package evcel.webjs

import org.scalajs.dom.raw.Element

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSName}
import js.JSConverters._

object MithrilElements {
  def ms(name:String, attributes:Map[String,Any]=Map())(contents: => Seq[Any]) = {
    js.Dynamic.global.m(name, attributes.toJSDictionary, contents.toJSArray)
  }
  def m(name:String, attributes:Map[String,Any]=Map())(contents: => Any) = {
    js.Dynamic.global.m(name, attributes.toJSDictionary, js.Array(contents))
  }
  def me(name:String, attributes:Map[String,Any]=Map()) = {
    js.Dynamic.global.m(name, attributes.toJSDictionary, js.Array())
  }
  def mt(name:String, attributes:Map[String,Any]=Map(), text:String) = {
    js.Dynamic.global.m(name, attributes.toJSDictionary, js.Array(text))
  }

  def f( x: js.Dynamic => Unit):js.Function = x

}
//@JSName("m")
//object Mithril extends js.Object {
//  def module(element:Element, module:MyModule):Unit = js.native
//}
