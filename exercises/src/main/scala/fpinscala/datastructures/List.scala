package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.1: the value of x above is 3.

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /*
    Exercise 3.8:
      foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
      returns (we set f = Cons(_, _) for this)
        Cons(1, foldRight(List(2, 3), Nil)(f)),
        Cons(1, Cons(2, foldRight(List(3), Nil)(f)))
        Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)(f))))
        Cons(1, Cons(2, Cons(3, Nil)))

      Therefore, foldRight(L, Nil)(Cons(_, _)) returns L.
      This implies that the constructor for List is essentially just a foldRight
      function.
   */

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
    Exercise 3.7: product2 will not immediately halt if the list contains 0.0.
    There is nothing in foldRight that performs a halt.
   */

  // Exercise 3.2:
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  // Exercise 3.3:
  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, l)

  // Exercise 3.4:
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case x if (n <= 0) => x
      case Cons(h, t) => drop(t, n-1)
    }

  // Exercise 3.5:
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case x => x
    }

  // Exercise 3.6: this cannot be in constant time because in order to remove
  // the last link of a linked list, one must traverse the entire list!
  def init[A](l: List[A]): List[A] =
    l match{
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, Cons(y, t)) => init(Cons(y, t))
    }

  // Exercise 3.9:
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => 1 + y)

  // Exercise 3.10:
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // Exercise 3.11.a:
  def sum3(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  // Exercise 3.11.b:
  def product3(as: List[Int]): Int =
    foldLeft(as, 1)(_ * _)

  // Exercise 3.11.c
  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, y) => 1 + x)

  // Exercise 3.12:
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

  /*
    Let's just see how reverse plays out on an example.
    reverse(List(1, 2, 3)) returns: (with f as the function in reverse)
      foldLeft(List(1, 2, 3), Nil)(f)
      foldLeft(List(2, 3), Cons(1, Nil))
      foldLeft(List(3), Cons(2, Cons(1, Nil)))
      foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil))))
      Cons(3, Cons(2, Cons(1, Nil))) == List(3, 2, 1)
   */

  // Exercise 3.13.a:
  /*
    Hint:
      It's possible to do both directions. For your `foldLeft` in terms of
      `foldRight`, you must build up, using `foldRight`, some value that you can
      use to achieve the effect of `foldLeft`. (It won't be the `B` of the
      return type necessarily)
   */
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => (b: B) => g(f(b, a)))(z)

  // Exercise 3.13.b:
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y, x)) // reverse is built from foldLeft

  // Exercise 3.14:
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((x, y) => Cons(x, y))

  def append3[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((x, y) => Cons(y, x))

  // Exercise 3.15:
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((x, y) => append3(x, y))

  // Exercise 3.16:
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((a, b) => Cons(a+1, b))

  /*
  def map[A,B](l: List[A])(f: A => B): List[B] = ???
  */
}
