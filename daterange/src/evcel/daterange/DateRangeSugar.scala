package evcel.daterange

import scala.language.implicitConversions

/**
 * Syntactic sugar for constructing date ranges and days in tests
 */
object DateRangeSugar {
  sealed trait MonthName {
    def monthNumber: Int

    def /(year: Int): Month = {
      val yearNo = if (year < 100) 2000 + year else year
      Month(yearNo, monthNumber)
    }
  }
  case object Jan extends MonthName { val monthNumber = 1 }
  case object Feb extends MonthName { val monthNumber = 2 }
  case object Mar extends MonthName { val monthNumber = 3 }
  case object Apr extends MonthName { val monthNumber = 4 }
  case object May extends MonthName { val monthNumber = 5 }
  case object Jun extends MonthName { val monthNumber = 6 }
  case object Jul extends MonthName { val monthNumber = 7 }
  case object Aug extends MonthName { val monthNumber = 8 }
  case object Sep extends MonthName { val monthNumber = 9 }
  case object Oct extends MonthName { val monthNumber = 10 }
  case object Nov extends MonthName { val monthNumber = 11 }
  case object Dec extends MonthName { val monthNumber = 12 }

  /**
   * Allows days to be written as (e.g.) 14 / Feb / 1966
   */
  implicit def daySugar(dayNo: Int) = new {
    def /(monthName: MonthName) = {
      require(dayNo <= 31, "incorrect construction of day of month daterange sugar")
      new {
        def /(year: Int) = {
          val yearNo = if (year < 100) 2000 + year else year // e.g. 1 / Nov / 13
          Day(yearNo, monthName.monthNumber, dayNo)
        }
      }
    }
  }
}
