package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(a) => 1
    case Branch(l,r) => 1+size(l)+size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(a) => 1
    case Branch(l,r) => 1+(depth[A](l) max depth[A](r))
  }

  def map[A,B](tree: Tree[A])(f: A=>B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map[A,B](l)(f),map[A,B](r)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A=>B)(g: (B,B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }
}