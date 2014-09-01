package com.evcel.core.qty

import org.scalatest.{ShouldMatchers, FunSuite}

class UOMRatioTest extends FunSuite with ShouldMatchers {

  test("test reduce") {
    val usd = UOMRatio(3, 1)
    val bbl = UOMRatio(5, 1)

    usd.reduce shouldEqual usd
    (usd / bbl).reduce shouldEqual usd/bbl
    (usd * bbl / usd).reduce shouldEqual bbl
    (usd * bbl * bbl / (usd * bbl)).reduce shouldEqual bbl
    (usd * usd * bbl * bbl / (usd * bbl)).reduce shouldEqual (usd*bbl)
  }

  test("test reduce 2") {
    val uom = UOMRatio(8576579,9713)
    uom.reduce shouldEqual UOMRatio(883, 1)
  }
}
