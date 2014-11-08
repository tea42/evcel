package evcel.utils

import java.lang.reflect.Method

import scala.util.control.NonFatal

class Enumerate[T](typ: Class[T], names: T => Seq[String]) {
  private lazy val fieldsAndValues: Seq[(String, T)] = try {
    def isFieldOfCorrectType(method: Method) = {
      method.getReturnType == typ && method.getParameterTypes.isEmpty
    }
    getClass.getDeclaredMethods.filter(isFieldOfCorrectType).flatMap(method => {
      val res = method.invoke(this).asInstanceOf[T]
      require(res != null, "Null return for " + method)
      names(res).map(_ -> res)
    }).toList
  } catch {
    case NonFatal(e) => throw new RuntimeException("Failed for " + this, e)
  }
  lazy val valuesByFieldName: Map[String, T] = fieldsAndValues.toMap
  lazy val values: Seq[T] = fieldsAndValues.map(_._2)

  def fromName(name: String):Option[T] = valuesByFieldName.get(name)
  def fromNameOrThrow(name: String): T = fromName(name).getOrElse(sys.error("Unknown: " + name))
}
