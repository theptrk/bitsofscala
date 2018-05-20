package chapter6_part1

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
    case class SimpleRNG(seed: Long) extends RNG {
        def nextInt: (Int, RNG)  = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }
    }

    // ex: 6.1 
    // Write a function that uses RNG.nextInt to generate a random integer 
    // between 0 and Int.maxValue (inclusive). Make sure to handle the corner 
    // case when nextInt returns Int.MinValue, which doesn’t have a non-negative 
    // counterpart.
    // 
    // Note: the Int data type is a two's complement with the absolute value of
    // Int.MinValue being 1 larger than Int.MaxValue. That means you should
    // convert -1 to 0, -2 to 1 and Int.MinValue to Int.MaxValue
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i < 0) -(i + 1) else i, r)
    }

    // ex: 6.2 
    // Write a function to generate a Double between 0 and 1, not including 1. 
    // Note: You can use Int.MaxValue to obtain the maximum positive integer 
    // value, and you can use x.toDouble to convert an x: Int to a Double.

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }

    // ex: 6.3
    // Write functions to generate an (Int, Double) pair, a (Double, Int) pair, 
    // and a (Double, Double, Double) 3-tuple. You should be able to reuse the 
    // functions you’ve already written.

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng1) = rng.nextInt
        val (d, rng2) = double(rng1)
        ((i, d), rng2)
    }
  
    // ex: 6.4
    // Write a function to generate a list of random integers.
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        if (count == 0)
            (List(), rng)
        else {
            val (x, r1)  = rng.nextInt
            val (xs, r2) = ints(count - 1)(r1)
            (x :: xs, r2)
    }
    def ints_tail_recursive(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = 
        if (count == 0)    
          (xs, r)
        else {
          val (x, rng2) = r.nextInt
          go(count-1, rng2, x :: xs)
        }
      
      go(count, rng, List())
    }
}
