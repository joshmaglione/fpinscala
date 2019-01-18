package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Exercise 3.25:
  def size[A](T: Tree[A]): Int =
    T match {
      case Leaf(x) => 1
      case Branch(x, y) => 1 + size(x) + size(y)
    }

  // Exercise 3.26:
  def maximum(T: Tree[Int]): Int =
    T match {
      case Leaf(x) => x
      case Branch(x, y) => maximum(x) max maximum(y)
    }

  // Exercise 3.27:
  def depth[A](T: Tree[A]): Int =
    T match {
      case Leaf(x) => 1
      case Branch(x, y) => (depth(x) max depth(y)) + 1
    }

  // Exercise 3.28:
  def map[A, B](T: Tree[A])(f: A => B): Tree[B] =
    T match {
      case Leaf(a) => Leaf(f(a))
      case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }

  // Exercise 3.29a:
  def fold[A, B](T: Tree[A])(g: A => B)(f: (B, B) => B): B =
    T match{
      case Leaf(x) => g(x)
      case Branch(x, y) => f(fold(x)(g)(f), fold(y)(g)(f))
    }

  // Exercise 3.29b:
  def size2[A](T: Tree[A]): Int =
    fold(T)((a: A) => 1)((x, y) => 1 + x + y)

  // Exercise 3.29c:
  def maximum2(T: Tree[Int]): Int =
    fold(T)((a: Int) => a)((x, y) => x max y)

  // Exercise 3.29d:
  def depth2[A](T: Tree[A]): Int =
    fold(T)((a: A) => 1)((x: Int, y: Int) => (x max y) + 1)

  // Exercise 3.29e:
  // I don't understand why these functions are types A => Leaf[B] and
  // (Tree[B], Tree[B]) => Branch[B].
  //
  //def map2[A, B](T: Tree[A])(f: A => B): Tree[B] =
  //  fold(T)((a: A) => Leaf(f(a)))((x: Tree[B], y: Tree[B]) => Branch(x, y))
}
