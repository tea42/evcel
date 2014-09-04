package com.evcel.instrument

import com.evcel.valuation.Valuer

trait Instrument {

  def valuer: Valuer
}
