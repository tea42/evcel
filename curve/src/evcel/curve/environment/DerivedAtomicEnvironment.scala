package evcel.curve.environment

import com.google.common.collect.Sets
import evcel.curve.ReferenceData
import scala.collection.JavaConverters._
import evcel.utils.EitherUtils._

abstract class DerivedAtomicEnvironment(original: AtomicEnvironment)
  extends AtomicEnvironment {
  override def marketDay = original.marketDay
}

case class ForwardStateEnvironment(refData: ReferenceData, original: AtomicEnvironment, forwardMarketDay: MarketDay)
  extends DerivedAtomicEnvironment(original) {
  override def marketDay = forwardMarketDay
  def apply(k: AtomicDatumIdentifier) = k.forwardStateValue(refData, original, forwardMarketDay)
}

case class PerturbedAtomicEnvironment(
  original: AtomicEnvironment,
  perturbation: PartialFunction[AtomicDatumIdentifier, Any])
    extends DerivedAtomicEnvironment(original) {

  def apply(k: AtomicDatumIdentifier) = if(perturbation.isDefinedAt(k))
    Right(perturbation.apply(k))
  else
    original.apply(k)
}

case class KeyRecordingAtomicEnvironment(original: AtomicEnvironment, refData: ReferenceData)
  extends DerivedAtomicEnvironment(original) {
  private val identifiers = Sets.newCopyOnWriteArraySet[AtomicDatumIdentifier]()

  def keys = identifiers.asScala.toSet

  def apply(k: AtomicDatumIdentifier) = {
    identifiers.add(k)
    original.apply(k).recover { case _ => k.nullValue(refData)}
  }
}