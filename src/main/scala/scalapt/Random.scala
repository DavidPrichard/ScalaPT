package scalapt

object RNG {
  final val Scale = Long.MaxValue.toDouble - Long.MinValue.toDouble + 1.0
}

trait RNG[T] {
  def next: T
}

//class DoubleRNG(rng: RNG[Long]) extends RNG[Double] {
//  import RNG._
//
//  def next: (DoubleRNG, Double) = {
//    val (rng2, rl) = rng.next
//    val dl = (rl.toDouble - Long.MinValue.toDouble) / Scale
//    (new DoubleRNG(rng2), dl)
//  }
//}

// XorShift64*, a relatively fast PRNG with better output than an LCG.
// Look into xoroshiro128+ for an upgrade: http://xoroshiro.di.unimi.it/
case class XorShift(var seed: Long) extends RNG[Long] {
  import RNG._

  def next: Long = {
    val a = seed ^ (seed >>> 12)
    val b = a ^ (a << 25)
    val c = b ^ (b >>> 27)
    seed = if (c == 0) -1 else c

    seed * 2685821657736338717L
  }

  // next with returned long scaled down to a double in [0,1]
  // (it may be better to find a PRNG that directly generates numbers in the unit interval)
  // this one may produce bias towards 0 given the varying precision of a Double.
  def next0to1: Double = (next.toDouble - Long.MinValue.toDouble) / Scale
}