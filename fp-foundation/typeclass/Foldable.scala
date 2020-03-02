package exercises.typeclass

trait Foldable[F[_]] {

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], z: B)(f: (A, => B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit ev: Monoid[B]): B =
    foldLeft(fa, ev.empty)((acc, a) => ev.combine(acc, f(a)))

  def reduceMap[A, B](fa: F[A])(f: A => B)(implicit ev: Semigroup[B]): Option[B] =
    foldLeft(fa, Option.empty[B])((acc, a) => Some(acc.fold(f(a))(ev.combine(_, f(a)))))

}

object Foldable {
  def apply[F[_]](implicit ev: Foldable[F]): Foldable[F] = ev

  object syntax {
    implicit class FoldableOps[F[_], A](self: F[A]) {
      def foldLeft[B](z: B)(f: (B, A) => B)(implicit ev: Foldable[F]): B          = ev.foldLeft(self, z)(f)
      def foldRight[B](z: B)(f: (A, => B) => B)(implicit ev: Foldable[F])         = ev.foldRight(self, z)(f)
      def foldMap[B: Monoid](f: A => B)(implicit ev: Foldable[F]): B              = ev.foldMap(self)(f)
      def reduceMap[B: Semigroup](f: A => B)(implicit ev: Foldable[F]): Option[B] = ev.reduceMap(self)(f)
    }
  }
}
