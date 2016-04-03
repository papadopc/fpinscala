package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h,t) => h()::t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n>1 => cons(h(),t().take(n-1))
    case Cons(h,t) if n==1 => cons(h(),empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n==0 => this
    case Cons(h,t) if n>0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)
  }

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A=>B):Stream[B] = {
    foldRight(empty[B])((h,t)=>cons(f(h),t))
  }

  def filter(p: A=> Boolean): Stream[A] = {
    foldRight(empty[A])( (h,t) => if (p(h)) cons(h,t) else t )
  }

  def append[B>:A](s: =>Stream[B]): Stream[B] = {
    foldRight(s)( (h,t) => cons(h,t) )
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])( (h,t) => f(h) append t )
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n,from(n+1))
  }

  val fibs: Stream[Int] = {
    def it(v0: Int, v1:Int): Stream[Int] = cons(v0, it(v1,v0+v1))
    it(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h,unfold(s)(f))
    case None => empty
  }

  val fibs2: Stream[Int] = {
    unfold( (0,1) )( s => Some(s._1+s._2,(s._2,s._1+s._2) ))
  }

  def from2(n: Int): Stream[Int] = {
    unfold( n )(s => Some( (s,s+1) ))
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)( s => Some( (a,a) ))
  }

  def ones2: Stream[Int] = {
    unfold(1)(_ => Some(1,1))
  }
}