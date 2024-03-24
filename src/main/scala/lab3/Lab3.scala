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
