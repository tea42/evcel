package evcel.utils

import java.lang.reflect.Method

import scala.util.control.NonFatal

/**
 * Extend to give an Object an Enumerable interface for all the member variables of type `typ`
 *
 * For example the UOM object could extend with type=classOf[UOM]. Then all the vals of type
 * UOM in the object would be accessible through this interface.
 *
 * The interface provides a way to list all the members of the object of type `typ`. Also gives
 * a method to create from strings.
 */
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
