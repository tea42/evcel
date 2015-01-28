package evcel.valuation

import evcel.curve.ValuationContext
import evcel.curve.environment.{MarketDay, PriceIdentifier}
import evcel.valuation.Valuer._
import evcel.instrument._
import evcel.quantity.{DblQty, Qty}
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, SingularValueDecomposition, RealMatrix}
import scala.util.{Either, Left, Right}
import evcel.utils.{EvcelFail, EitherUtils}
import evcel.utils.EitherUtils._
import scala.collection.immutable.Nil
import evcel.daterange.Month

object SVDPositions{
  private def calcJacobian(
    vc : ValuationContext,
    valuer : Valuer,
    portfolio : Seq[Instrument],
    priceKeys : Seq[PriceIdentifier]
  ) : Either[EvcelFail, Array2DRowRealMatrix] = {
    val matrix = new Array2DRowRealMatrix(priceKeys.size, portfolio.size)
    var maybeFailure : Option[EvcelFail] = None

    for {
      (key, i) <- priceKeys.zipWithIndex
      (inst, j) <- portfolio.zipWithIndex
    } {
      // Do no more calculations after the first failure
      if (!maybeFailure.isDefined){
        valuer.firstOrderPriceDiff(vc, inst, key) match {
          case Left(e) => 
            maybeFailure = Some(e)
          case Right(delta) => 
            matrix.setEntry(i, j, delta.doubleValue)
        }
      }
    }

    maybeFailure.toLeft(matrix)
  }
  private def portfolioDeltas(
    vc : ValuationContext, 
    valuer : Valuer,
    portfolio : Seq[Instrument],
    priceKeys : Seq[PriceIdentifier]) : Either[EvcelFail, Array2DRowRealMatrix] = {
      val jacobian = calcJacobian(vc, valuer, portfolio, priceKeys)
      jacobian.map{
        j => 
          val deltas = new Array2DRowRealMatrix(j.getRowDimension(), 1)
          for (i <- 0 until j.getRowDimension()){
            deltas.setEntry(i, 0, j.getRow(i).sum)
          }
          deltas
      }
  }

  // TODO - change `inst` to `portfolio`
  def positions(
    vc: ValuationContext, 
    valuer : Valuer, 
    inst : Instrument,
    hedgingInstruments : Seq[HedgeInstrument]
  ) : Map[HedgeInstrument, Either[EvcelFail, Double]] = { 
    val positions : Map[HedgeInstrument, Either[EvcelFail, Double]] = {
      val instrumentKeys = (hedgingInstruments :+ inst).flatMap(valuer.priceKeys(vc, _)).distinct

      val allKeys = instrumentKeys.sortWith{
        case (p1:PriceIdentifier, p2: PriceIdentifier) => p1.point.firstDay < p2.point.firstDay
      }
      val solution : Either[EvcelFail, RealMatrix] = for {
        jacobian <- calcJacobian(vc, valuer, hedgingInstruments, allKeys)
        hedgePos <- portfolioDeltas(vc, valuer, Vector(inst), allKeys)
      } yield {
        val svd = new SingularValueDecomposition(jacobian)
        svd.getSolver.solve(hedgePos)
      }
      solution match {
        case Left(fail) => 
          hedgingInstruments.map{h => h -> Left(fail)}(scala.collection.breakOut)
        case Right(matrix) => 
          hedgingInstruments.zipWithIndex.map {
            case (hedge, i) =>
              (hedge, Right(matrix.getEntry(i, 0)))
          }(scala.collection.breakOut)
      }
    }
    combineHedgesByTenor(vc, positions)
  }

  def combineHedgesByTenor(
    vc : ValuationContext, 
    hedges : Map[HedgeInstrument, Either[EvcelFail, Double]]
  ) : Map[HedgeInstrument, Either[EvcelFail, Double]] = {

    vc.params.tenor match {
      case Some(Month) => 
        val swapHedges = scala.collection.mutable.Map[CommoditySwap, Double]()
        val otherHedges = scala.collection.mutable.Map[HedgeInstrument, Either[EvcelFail, Double]]()
        hedges.foreach{
          case (s : CommoditySwap, Right(d)) =>
            swapHedges += (s -> d)
          case (h, e) => 
            otherHedges += (h -> e)
        }

        val combinedDailySwaps = swapHedges.groupBy{
          case (swap, _) => (swap.averagingPeriod.asDay.containingMonth, swap.index, swap.level)
        }.map{
          case ((month, index, level), swaps) =>
            // The 'right.get' looks dodgy, however to have constructed hedges using this index
            // we know the index must exist
            val richIndex = RichIndex(vc.refData, index, level).right.get
            val totalDailyHedgeAmount = swaps.map{case (_, hedgeAmount) => hedgeAmount}.sum
            val totalMonthlyHedgeAmount = 
              totalDailyHedgeAmount * richIndex.volumeCalcRule.scaleHedgeAmountFromDaily(month)
            (richIndex.unitHedge(month), Right(totalMonthlyHedgeAmount))
        }
        (otherHedges ++ combinedDailySwaps).toMap
      case _ => 
        hedges
    }
  }
}
