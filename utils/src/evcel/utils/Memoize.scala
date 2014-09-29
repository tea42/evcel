package evcel.utils

import java.util.concurrent.ConcurrentHashMap

trait Memoize[K, V] extends (K => V){
  protected def singleParameterFunction(k : K) : V

  private val cache = new ConcurrentHashMap[K, V]()
  def update(key : K) : V = {
    val v = singleParameterFunction(key)
    cache.put(key, v)
    v
  }
  def apply(key : K): V = {
    Option(cache.get(key)).getOrElse(update(key))
  }
}

object Memoize{
  def apply[T, R](f: T => R) = new Memoize[T, R]{
    def singleParameterFunction(t : T) = f(t)
  }

  def apply[T1, T2, R](f: (T1, T2) => R) = new Memoize[(T1, T2), R]{
    def singleParameterFunction(t : (T1, T2)) = f(t._1, t._2)
  }

  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R) = new Memoize[(T1, T2, T3), R]{
    def singleParameterFunction(t : (T1, T2, T3)) = f(t._1, t._2, t._3)
  }
}