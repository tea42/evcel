package com.evcel.valuation

import com.evcel.Environment.Environment
import com.evcel.core.qty.{ Qty, UOM }

trait Valuer {
  def value(env: Environment, ccy: UOM): Qty
}
