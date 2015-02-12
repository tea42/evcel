package evcel.maths.models

import evcel.daterange._
import evcel.maths.BarrierType._
import evcel.maths._
import evcel.maths.utils.DoubleTestUtils
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest._

class MertonReinerRubinsteinBarrierTest extends FunSuite with Matchers {
  test("against ss 1") {
    val S = 100
    val k = 3 // rebate
    val T = 0.5
    val r = 0.08
    val b = 0.04
    results1.lines.foreach {
      line =>
        val (cp, barrier, x, h, value1, value2) = parseLine(line)
        val barrier25 = MertonReinerRubinsteinBarrier(Continuously, cp, barrier, S, x, h, T, .25, k, r, b)
        val barrier30 = MertonReinerRubinsteinBarrier(Continuously, cp, barrier, S, x, h, T, .30, k, r, b)

        barrier25.value shouldEqual (value1 +- 1e-7)
        barrier30.value shouldEqual (value2 +- 1e-7)
    }
  }

  test("against ss 2") {
    val S = 100
    val k = 3 // rebate
    val T = 0.1
    val r = 0.08
    val b = 0.0
    results2.lines.foreach {
      line =>
        val (cp, barrier, x, h, value1, value2) = parseLine(line)
        val barrier15 = MertonReinerRubinsteinBarrier(Continuously, cp, barrier, S, x, h, T, .15, k, r, b)
        val barrier55 = MertonReinerRubinsteinBarrier(Continuously, cp, barrier, S, x, h, T, .55, k, r, b)

        barrier15.value shouldEqual (value1 +- 1e-7)
        barrier55.value shouldEqual (value2 +- 1e-7)
    }
  }

  test("like vanilla option depending on barrier") {
    implicit val equality = new DoubleTestUtils.AlmostEqualsPC(tolPC = 0.001, min = 0.001)

    for (cp <- List(Call, Put); typ <- List(DownAndIn, DownAndOut, UpAndIn, UpAndOut);
      t <- List(0.1, 0.35, 0.6, 1.5, 3.0); r <- List(0.05, 0.25);
      k <- List(80, 95, 100, 105, 110, 120); vol <- List(0.1, 0.3, 0.6)) {

      val H = typ match {
        case DownAndIn | UpAndIn => 100
        case DownAndOut => 1
        case UpAndOut => 10000
      }
      val barrier = MertonReinerRubinsteinBarrier(Continuously, cp, typ, 100, k, H, t, vol, 0, r, 0).value
      val bs = new BlackScholes(cp, 100, k, vol, t).undiscountedValue * math.exp(-r * t)

      bs shouldEqual barrier
    }
  }

  test("like vanilla option when H = S") {
    implicit val equality = new DoubleTestUtils.AlmostEqualsPC(tolPC = 0.001, min = 0.001)

    for (cp <- List(Call, Put); typ <- List(DownAndIn, DownAndOut, UpAndIn, UpAndOut);
      t <- List(0.1, 0.35, 0.6, 1.5, 3.0); r <- List(0.05, 0.25);
      k <- List(80, 95, 100, 105, 110, 120); vol <- List(0.1, 0.3, 0.6)) {

      val S = 100
      val H = S

      val barrier = MertonReinerRubinsteinBarrier(Continuously, cp, typ, 100, k, H, t, vol, 0, r, 0).value
      val value = if(typ.inOrOut == BarrierOut) {
        0.0
      } else {
        new BlackScholes(cp, 100, k, vol, t).undiscountedValue * math.exp(-r * t)
      }

      value shouldEqual barrier
    }
  }

  test("parity") {
    for (cp <- List(Call, Put); typ <- List(DownAndIn, DownAndOut, UpAndIn, UpAndOut);
      t <- List(0.1, 0.35, 0.6, 1.5, 3.0); r <- List(0.05, 0.25);
      k <- List(80, 95, 100, 105, 110, 120); h <- List(80, 95, 100, 105, 110, 120); vol <- List(0.1, 0.3, 0.6)) {

      val S = 100.0
      val barrier1 = MertonReinerRubinsteinBarrier(Continuously, cp, typ, S, k, h, t, vol, 0, r, 0).value
      val otherType = if(typ.inOrOut == BarrierIn) typ.copy(inOrOut = BarrierOut) else typ.copy(inOrOut = BarrierIn)
      val barrier2 = MertonReinerRubinsteinBarrier(Continuously, cp, otherType, S, k, h, t, vol, 0, r, 0).value

      val bs = new BlackScholes(cp, S, k, vol, t).undiscountedValue * math.exp(-r * t)

      bs shouldEqual ((barrier1 + barrier2) +- 1e-7)
    }
  }

  test("discrete time adjustment for H") {
    import evcel.maths.models.MertonReinerRubinsteinBarrier._
    adjustH(S = 100, H = 110, v = .25, periodicity = Continuously) shouldEqual 110
    adjustH(S = 100, H = 110, v = .25, periodicity = Hourly) shouldEqual (110.17131251 +- 1e-7)
    adjustH(S = 100, H = 110, v = .25, periodicity = Daily) shouldEqual (110.84180839 +- 1e-7)
    adjustH(S = 100, H = 110, v = .25, periodicity = Weekly) shouldEqual (112.24437193 +- 1e-7)
    adjustH(S = 100, H = 110, v = .25, periodicity = Monthly) shouldEqual (114.72361629 +- 1e-7)

    adjustH(S = 120, H = 110, v = .25, periodicity = Continuously) shouldEqual 110
    adjustH(S = 120, H = 110, v = .25, periodicity = Hourly) shouldEqual (109.82895387 +- 1e-7)
    adjustH(S = 120, H = 110, v = .25, periodicity = Daily) shouldEqual (109.16458487 +- 1e-7)
    adjustH(S = 120, H = 110, v = .25, periodicity = Weekly) shouldEqual (107.80050520 +- 1e-7)
    adjustH(S = 120, H = 110, v = .25, periodicity = Monthly) shouldEqual (105.47087331 +- 1e-7)
  }

  test("against MC") {
    val iterations = 10 * 1000
    val mt = new MersenneTwister(1234)
    for (vol <- List(0.3); b <- List(0.05, 0.0); t <- List(0.35, 0.6)) {
      val s = 100.0
      val params = new MCParams(b = b, S = s, T = t, vol = vol)
      val timeSteps = (t * 365).round.toInt // Daily

      val mc = new MonteCarlo(iterations, timeSteps, mt, params)
      for (cp <- List(Call, Put); typ <- List(DownAndIn, DownAndOut, UpAndIn, UpAndOut);
        r <- List(0.05, 0.0);
        k <- List(95, 100, 105, 110); h <- List(95, 100, 105, 110)) {
        val rebate = .25
        val valuation = BarrierOptionMC.valuation(cp, typ, s, k, h, rebate)
        val bv = MertonReinerRubinsteinBarrier(Daily, cp, typ, 100, k, h, t, vol, rebate, r, b).value
        val (mcv, se) = mc.value(valuation, math.exp(-r * t))
        bv shouldEqual (mcv +- (se * 2).max(0.1))
        se should be < (bv * 0.05).max(0.01)
      }
    }
  }

  test("cost of carry") {
    val iterations = 10 * 1000
    val mt = new MersenneTwister(1234)

    val cp = Call
    val typ = UpAndOut
    val rebate = .25
    val S = 100
    val K = 50
    val H = 120
    val T = 0.5
    val vol = 0.3
    val r = 0.30
    val rf = 0.15
    val b = r - rf // nice big cost of carry. makes a huge difference to the valuation

    val bv = MertonReinerRubinsteinBarrier(Daily, cp, typ, S, K, H, T, vol, rebate, r, b).value

    val params = new MCParams(b = b, S = S, T = T, vol = vol)
    val timeSteps = (T * 365).round.toInt // Daily
    val valuation = BarrierOptionMC.valuation(cp, typ, S, K, H, rebate)

    val mc = new MonteCarlo(iterations, timeSteps, mt, params)
    val (mcv, se) = mc.value(valuation, math.exp(-r * T))

    bv should be > 5.0
    bv shouldEqual (mcv +- (se * 2).max(0.1))
    se should be < (bv * 0.01).max(0.01)
  }

  private def parseLine(line: String) = {
    val typ :: x :: h :: value1 :: value2 :: Nil = line.split("\t").toList

    val (cp, barrier) = typ match {
      case "cdo" => (Call, DownAndOut)
      case "cdi" => (Call, DownAndIn)
      case "cui" => (Call, UpAndIn)
      case "cuo" => (Call, UpAndOut)
      case "pdo" => (Put, DownAndOut)
      case "pdi" => (Put, DownAndIn)
      case "pui" => (Put, UpAndIn)
      case "puo" => (Put, UpAndOut)
    }

    (cp, barrier, x.toDouble, h.toDouble, value1.toDouble, value2.toDouble)
  }

  // Results from Haug barrier spreadsheet
  // S = 100, K  = 3, T = 0.5, r = 0.08, b = 0.04
  // Type, X, H, Vol = 0.25, Vol = 0.3
  val results1 =
    """|cdo	90	95	9.024567695	8.833357929
      |cdo	100	95	6.792436575	7.028540222
      |cdo	110	95	4.87585774	5.41369998
      |cdo	90	100	3	3
      |cdo	100	100	3	3
      |cdo	110	100	3	3
      |cuo	90	105	2.678912505	2.634041951
      |cuo	100	105	2.358019791	2.438941885
      |cuo	110	105	2.345348946	2.431532679
      |cdi	90	95	7.76267021	9.009344381
      |cdi	100	95	4.01094185	5.137038583
      |cdi	110	95	2.057612753	2.851682785
      |cdi	90	100	13.8332871	14.8816208
      |cdi	100	100	7.849427622	9.2044973
      |cdi	110	100	3.97951969	5.30430126
      |cui	90	105	14.11117312	15.20984591
      |cui	100	105	8.448206354	9.727822476
      |cui	110	105	4.590969266	5.835035642
      |pdo	90	95	2.279837967	2.416990337
      |pdo	100	95	2.294749633	2.425809856
      |pdo	110	95	2.625213585	2.62460684
      |pdo	90	100	3	3
      |pdo	100	100	3	3
      |pdo	110	100	3	3
      |puo	90	105	3.775955132	4.229237465
      |puo	100	105	5.493227672	5.803252006
      |puo	110	105	7.518722082	7.564957407
      |pdi	90	95	2.958582131	3.876894166
      |pdi	100	95	6.567705377	7.798845533
      |pdi	110	95	11.97522788	13.3077469
      |pdi	90	100	2.284469295	3.332802998
      |pdi	100	100	5.908504207	7.263573885
      |pdi	110	100	11.64649067	12.97127224
      |pui	90	105	1.465312685	2.065832594
      |pui	100	105	3.372075057	4.422588939
      |pui	110	105	7.084567106	8.36858189""".stripMargin

  // Results from Haug barrier spreadsheet
  // S = 100, K  = 3, T = 0.1, r = 0.08, b = 0.0
  // Type, X, H, Vol = 0.15, Vol = 0.55
  val results2 =
    """|cdo	90	95	9.354452022	8.373006584
      |cdo	100	95	2.708032169	6.280263387
      |cdo	110	95	0.897330858	4.521603646
      |cdo	90	100	3	3
      |cdo	100	100	3	3
      |cdo	110	100	3	3
      |cuo	90	105	6.416737663	2.640136772
      |cuo	100	105	1.258263057	2.28906059
      |cuo	110	105	0.885142138	2.275606229
      |cdi	90	95	3.565568319	7.267828767
      |cdi	100	95	2.148232864	3.585426256
      |cdi	110	95	2.122699862	1.781508895
      |cdi	90	100	9.940850318	12.64980665
      |cdi	100	100	1.877095011	6.874660938
      |cdi	110	100	0.040860698	3.312083836
      |cui	90	105	6.503501371	13.00033462
      |cui	100	105	3.59822067	7.57626509
      |cui	110	105	2.135107276	4.02714235
      |pdo	90	95	0.856486385	2.358301249
      |pdo	100	95	1.285678659	2.374649573
      |pdo	110	95	6.550589475	2.725081355
      |pdo	90	100	3	3
      |pdo	100	100	3	3
      |pdo	110	100	3	3
      |puo	90	105	0.905664347	4.025003776
      |puo	100	105	2.728011668	6.057455974
      |puo	110	105	9.335712675	8.427529993
      |pdi	90	95	2.143214808	3.362214953
      |pdi	100	95	3.570586375	7.491040069
      |pdi	110	95	6.389760394	13.49835033
      |pdi	90	100	0.02053117	2.729487497
      |pdi	100	100	1.877095011	6.874660938
      |pdi	110	100	9.961179846	13.23240298
      |pui	90	105	2.094255538	1.695148463
      |pui	100	105	2.128472059	3.807869706
      |pui	110	105	3.604855887	7.795537734""".stripMargin
}
