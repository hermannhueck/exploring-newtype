package francistoth

trait Associative[A] {
  def combine(a0: A, a1: A): A
}

object Associative {

  def apply[A: Associative]: Associative[A] = implicitly[Associative[A]]

  def reduce[A: Associative](zero: A, as: List[A]): A =
    as.fold(zero)(Associative[A].combine)
}
