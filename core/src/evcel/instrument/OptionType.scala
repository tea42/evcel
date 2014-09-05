package com.evcel.instrument

sealed trait OptionType

case object EuropeanOption extends OptionType
case object AmericanOption extends OptionType
case object AsianOption extends OptionType
