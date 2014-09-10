package evcel.curve.environment

import com.google.common.collect.Sets
import evcel.curve.ReferenceData
import evcel.curve.curves.{MissingCurveDataException, MissingCurveException}
import scala.collection.JavaConverters._

abstract class DerivedAtomicEnvironment(original: AtomicEnvironment)
  extends AtomicEnvironment {
  override def marketDay = original.marketDay
}

case class PerturbedAtomicEnvironment(
  original: AtomicEnvironment,
  perturbation: PartialFunction[AtomicDatumIdentifier, Any])
    extends DerivedAtomicEnvironment(original) {

  def apply(k: AtomicDatumIdentifier) = perturbation.applyOrElse(k, original.apply)
}

case class KeyRecordingAtomicEnvironment(original: AtomicEnvironment, refData: ReferenceData)
  extends DerivedAtomicEnvironment(original) {
  private val identifiers = Sets.newCopyOnWriteArraySet[AtomicDatumIdentifier]()

  def keys = identifiers.asScala.toSet

  def apply(k: AtomicDatumIdentifier) = {
    identifiers.add(k)
    try {
      original(k)
    } catch {
      case _: MissingCurveException => k.nullValue(refData)
      case _: MissingCurveDataException => k.nullValue(refData)
    }
  }
}