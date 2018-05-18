package chapter6_part1

import org.scalatest._

class IndexSpec extends FlatSpec with Matchers {

  // ex: 6.1
  "SimpleRNG" should "return a random int based on a seed" in {
    val (a, rng) = RNG.SimpleRNG(42).nextInt
    a shouldEqual 16159453
  }
  "SimpleRNG.nextInt" should "be able to return a negative int" in {
    val (a, rng) = RNG.SimpleRNG(8888).nextInt
    a shouldEqual -875319072
  }

  // ex: 6.2
  "nonNegativeInt" should "return a non negative int" in {
    // note that we are normalizing ints in a special way where
    // negative numbers are ran through i => -(i+1)
    val (a, rng) = RNG.nonNegativeInt(RNG.SimpleRNG(8888))
    a shouldEqual 875319071
  }
  // how would i test that given, minvalue nonnegative returns maxvalue???
  // how would i test that given, minvalue nonnegative returns maxvalue???
  // how would i test that given, minvalue nonnegative returns maxvalue???
  // how would i test that given, minvalue nonnegative returns maxvalue???
  // how would i test that given, minvalue nonnegative returns maxvalue???
  "nonNegativeInt" should "return Int.MaxValue for Int.MinValue" in {
    // note that we are normalizing ints in a special way where
    // negative numbers are ran through i => -(i+1)
  }

  // ex: 6.3
  "intDouble" should "return a (Int, Double) pair" in {
    // note that we are normalizing ints in a special way where
    // negative numbers are ran through i => -(i+1)
    val (a, rng) = RNG.intDouble(RNG.SimpleRNG(42))
    a shouldEqual (16159453,0.5967354848980904)
  }

  // ex: 6.4
  "ints" should "return a list of correct length" in {
    val (a, rng) = RNG.ints(3)(RNG.SimpleRNG(42))
    a.length shouldEqual 3
  }
  "ints" should "return a list of random ints" in {
    // this test is correct if the list is populated from left to right
    // this test could be wrong if the function appends to the list
    // from the right side to the left 
    val (a, rng) = RNG.ints(3)(RNG.SimpleRNG(42))
    a shouldEqual List(16159453, -1281479697, -340305902)
  }
  "ints_tail_recursive" should "return a list of random ints" in {
    // this test is correct if the list is populated from right to left
    // this test could be wrong if the function appends to the list
    // from the left side to the right 
    val (a, rng) = RNG.ints_tail_recursive(3)(RNG.SimpleRNG(42))
    a shouldEqual List(-340305902, -1281479697, 16159453)
  }

  // ex: 6.x
  "placeholder" should "lorem" in {
    100 shouldEqual 100
  }
}

