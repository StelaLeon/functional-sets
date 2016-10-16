package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = union(union(singletonSet(2),singletonSet(3)),singletonSet(4))
    val s5 = union(union(singletonSet(4),singletonSet(5)),singletonSet(6))

    val s6 = union(union(singletonSet(2),singletonSet(4)),singletonSet(6))
    val s7 = union(union(singletonSet(8),singletonSet(10)),singletonSet(12))
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("singletonSet(2) does not contain other values than '2' "){
    new TestSets {
      assert(!contains(s2,1),"Unwanted values in the set")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  test("intersect contains common elements of the set") {
    new TestSets {
      val s = intersect(s4,s5)
      assert(contains(s,4),"intersection contains the right element")
      assert(!contains(s,3), "intersection does not contain elements that are not common")
      assert(!contains(s,2), "intersection does not contain elements that are not common")
      assert(!contains(s,5), "intersection does not contain elements that are not common")
      assert(!contains(s,6), "intersection does not contain elements that are not common")

    }
  }
  test("diff s4-s5 contains elements that are not in s5 and are included in s4") {
    new TestSets {
      val s = diff(s4,s5)
      assert(!contains(s,4),"intersection contains the right element")
      assert(contains(s,3), "intersection does not contain elements that are not common")
      assert(contains(s,2), "intersection does not contain elements that are not common")
    }
  }
  test("diff s5-s4 contains elements that are not in s4 and are included in s5") {
    new TestSets {
      val s = diff(s5,s4)
      assert(!contains(s,4),"intersection contains the right element")
      assert(contains(s,5), "intersection does not contain elements that are not common")
      assert(contains(s,6), "intersection does not contain elements that are not common")
    }
  }

  test("filter the elements from a set") {
    new TestSets {
      val s = filter(union(s5,s4),x=>x<=2)
      assert(contains(s,2),"filter should contain the desired values")
      assert(!contains(s,3), "filter should not contain the unwanted values")
      assert(!contains(s,4), "filter should contain the desired values")
      assert(!contains(s,5),"filter should not contain the unwanted values")
      assert(!contains(s,6), "filter should contain the desired values")
    }
  }

  test("for all false whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val s = forall(union(s5,s4),x=> x>3)
      assert(!s,"for all: condition should not be true for every value within the set")
    }
  }

  test("for all true whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val set = union(s6,s7)
      val s = forall(set,x=>x>1)
      assert(s,"for all: condition should be true for every value within the set")
    }
  }
  test("if it exists one elem within `s` satisfying `p`") {
    new TestSets {
      val set = union(s6,s7)
      val s = exists(set,x=>x>10)
      assert(s," at least one element accomplishes the requirement")
      }
  }

  test("it doesn't exist any elem within `s` satisfying `p`") {
    new TestSets {
      val set = union(s6,s7)
      val s = exists(set,x=>x>12)
      assert(!s,"no element accomplishes the requirement")
    }
  }
  test("verifying -1000") {
    new TestSets {
      val set = singletonSet(-1000)
      val s = exists(set,x=>x>12)
      assert(!s,"verifying -1000")
    }
  }

  test("verifying 13>12") {
    new TestSets {
      val set = singletonSet(13)
      val s = exists(set,x=>x>12)
      assert(s,"verifying 13>12")
    }
  }

  test("verifying map"){
    new TestSets {
      val set = singletonSet(2)
      val s = map(set,x=>x*2)
      assert(contains(s,4),"verying map(2, 2*2)")
    }
  }
  test("verifying map (2),(3),(4) *2"){
    new TestSets {
      val s = map(s4,x=>x*2)
      assert(contains(s,4),"verying map(2, 2*2)")
      assert(contains(s,6),"verying map(3, 3*2)")
      assert(contains(s,8),"verying map(4, 4*2)")
    }
  }
}
