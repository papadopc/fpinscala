package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextI,nextRNG) = rng.nextInt
    (if (nextI<0) -(nextI+1) else nextI, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextD,nextRNG) = this.nonNegativeInt(rng)
    (nextD/(Int.MaxValue+1),nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextI, tmpRNG) = rng.nextInt
    val (nextD, nextRNG) = this.double(tmpRNG)
    ((nextI,nextD),nextRNG)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (nextD, tmpRNG) = this.double(rng)
    val (nextI, nextRNG) = tmpRNG.nextInt
    ((nextD,nextI),nextRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (firstD, firstrng) = this.double(rng)
    val (secondD, secondrng) = this.double(firstrng)
    val (thirdD, thirdrng) = this.double(secondrng)
    ((firstD,secondD,thirdD),thirdrng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List(),rng)
    else {
      val (nextI, nextrng) = rng.nextInt
      val (endI, finalrng) = ints(count-1)(nextrng)
      (nextI::endI,finalrng)
    }
  }

  val doubleUsingMap: Rand[Double] = {
    map(nonNegativeInt)( _/(Int.MaxValue.toDouble+1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b),rng2)
    }

  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
