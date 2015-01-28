package evcel.curve

import evcel.daterange.{Day, Month}
import evcel.referencedata.Level
import evcel.referencedata.market.{FuturesContractIndex, FuturesContractIndexLabel, Index, TestMarkets}
import evcel.utils.EitherTestPimps
import org.scalatest.{FunSuite, ShouldMatchers}

class RichIndexTest extends FunSuite with ShouldMatchers with EitherTestPimps{

   test("test FFPI to FCI") {
     val refData = TestMarkets.testRefData
     val wti10 = TestMarkets.NYMEX_WTI_1st_MONTH
     val wti11 = wti10.copy(label = wti10.label.copy(rollEarlyDays = 1))
     val wti20 = wti10.copy(label = wti10.label.copy(nearby = 2))
     val wti21 = wti10.copy(label = wti10.label.copy(nearby = 2, rollEarlyDays = 1))

     def fci(month: Month)= {
       FuturesContractIndex(FuturesContractIndexLabel("Nymex WTI", month), TestMarkets.NYMEX_WTI, Level.Close)
     }

     val exp = refData.futuresExpiryRule("Nymex WTI").R
     val cal = refData.calendar("Nymex WTI").R
     val month = Month(2015, 1)
     val ltd = exp.futureExpiryDay(month).R

     def test(ndx: Index, observationDay: Day,
              observedMonth: Month, observedOnPrevDay: Month, observedOnNextDay: Month) = {
       val richIndex = RichIndex(refData, ndx).R
       richIndex.observable(refData, observationDay) shouldEqual Right(fci(observedMonth))
       richIndex.observable(refData, observationDay.previous) shouldEqual Right(fci(observedOnPrevDay))
       richIndex.observable(refData, observationDay.next) shouldEqual Right(fci(observedOnNextDay))
     }

     test(wti10, ltd, month, observedOnPrevDay = month, observedOnNextDay = month + 1)
     test(wti11, ltd, month + 1, observedOnPrevDay = month, observedOnNextDay = month + 1)
     test(wti11, cal.addBusinessDays(ltd, -1), month, observedOnPrevDay = month, observedOnNextDay = month + 1)

     test(wti20, ltd, month + 1, observedOnPrevDay = month + 1, observedOnNextDay = month + 2)
     test(wti21, ltd, month + 2, observedOnPrevDay = month + 1, observedOnNextDay = month + 2)
     test(wti21, cal.addBusinessDays(ltd, -1), month + 1, observedOnPrevDay = month + 1, observedOnNextDay = month + 2)
   }
 }
