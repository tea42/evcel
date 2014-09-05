package evcel.curve.environment

case class PerturbedAtomicEnvironment(
  original: AtomicEnvironment,
  perturbation: PartialFunction[AtomicDatumIdentifier, Any])
    extends AtomicEnvironment {
  def marketDay = original.marketDay
  def apply(k: AtomicDatumIdentifier) = perturbation.applyOrElse(k, original.apply)
}
