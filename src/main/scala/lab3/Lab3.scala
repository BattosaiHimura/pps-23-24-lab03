package lab3

import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import lab3.Lab3.take

object Lab3:

    // Task 1: svolto in autonomia
    def take[A](l: Sequence[A], n: Int): Sequence[A] = l match
      case Cons(h, t) if (n > 0) => Cons(h, take(t, n - 1))
      case _ => Nil()

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), _) => Cons(h, concat(t, l2))
      case (_, l2) => l2
