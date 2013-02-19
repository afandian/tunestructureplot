import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.specs.runner.JUnitSuiteRunner

/**
 * Joe Wass
 * 2013
 * joe@afandian.com
 */

import org.scalatest.{FlatSpec, FunSuite}
import org.specs.runner.JUnitSuiteRunner
import org.junit.runner.RunWith
import melodysequence.Functions

@RunWith(classOf[JUnitSuiteRunner])
class FirstSpec extends FlatSpec {
  "longestPrefix" should "Find Longest prefixes" in {
    val inp = List(1,2,3,4,1,2,3,4)
    assert(Functions.longestPrefix(inp, 0, 4) == 4)
    assert(Functions.longestPrefix(inp, 1, 5) == 3)
    assert(Functions.longestPrefix(inp, 2, 6) == 2)
    assert(Functions.longestPrefix(inp, 3, 7) == 1)
    assert(Functions.longestPrefix(inp, 4, 8) == 0)
  }

  "prefixForSearchIndex" should "Location of longest prefixes for identical sequences" in {
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,5,1,2,3,4,5), 0, 3) ==
      List((0, 5, 5)))
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,1,2,3,4), 0, 3) ==
      List((0, 4, 4)))
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,1,2,3), 0, 3) ==
      List((0, 3, 3)))
  }

  "prefixForSearchIndex" should "Location of longest prefixes for sequences" in {
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,1,2,3,4,5), 0, 3) ==
      List((0, 4, 4)))
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,1,2,3), 0, 3) ==
      List((0, 4, 3)))
  }

  "prefixForSearchIndex" should "Location of all longest prefixes for sequences" in {
    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,1,2,3,4,1,2,3,4), 0, 3) ==
      List((0,4,4), (0,8,4)))

    assert(Functions.prefixForSearchIndex(
      List(1,2,3,4,1,2,3,4,1,2,3), 0, 3) ==
      List((0,4,4), (0,8,3)))
  }
}