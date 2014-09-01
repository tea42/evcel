package com.evcel.core.util

import org.apache.commons.math3.primes.{Primes => APrimes}
import scala.annotation.tailrec

object Primes {
  def primes(from: Int = 2): Iterator[Int] = {
    Iterator.iterate(APrimes.nextPrime(from))(prev => APrimes.nextPrime(prev + 1))
  }

  def factor(number: Long, possiblePrimes: List[Int]) = {
    require(number > 1, "Invalid value: " + number)
    @tailrec
    def find(n: Long, factors: List[Int]): List[Int] = if (n == 1) {
      factors
    } else {
      possiblePrimes.find(prime => n % prime == 0) match {
        case Some(factor) => find(n / factor, factor::factors)
        case _ => Nil
      }
    }
    find(number, Nil)
  }
}
