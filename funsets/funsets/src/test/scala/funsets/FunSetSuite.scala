package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(6)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
 // @Ignore("not ready yet") 
  @Test def `singleton set one contains one`: Unit = {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  @Test def `intersection contains elements both from s1 and s2`: Unit = {
    new TestSets {
      val test = union(s1,s2) //1,2
      val s = intersect(test, s2)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  @Test def `diff contains elements from s which are not in t`: Unit = {
    new TestSets {
      val test = union(s1,s2) //1,2
      val s = diff(test, s2)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
   @Test def `filter returns the elements of s for which p(s) is true`: Unit = {
    new TestSets {
      val test = union(s3,union(s1,s2)) 
      val s = filter (test, x => x%2 != 0)
      assert(contains(s, 1))
      assert(!contains(s, 2))
      assert(contains(s, 3))
    }
  }
    @Test def `forall is true if for every element of s p(s) is true`: Unit = {
    new TestSets {
      val test = union(s3,union(s1,s2)) 
      assert(forall(test, x => x%2 != 0)==false)
      assert(forall(test, x => x>0) == true)
    }
  }
  @Test def `exists is true if there is an element of s p(s) is true`: Unit = {
    new TestSets {
      val test = union(s3,union(s1,s2)) 
      assert(exists(test, x => x%2 != 0)==true)
      assert(forall(test, x => x<0) == false)
    }
  }
  @Test def `map returns new set transformed by applying f to each element of s`: Unit = {
    new TestSets {
     val test = union(s3,union(s1,s2)) //1,2,3
     val mapp = map(test,x => 1)
     val mapp2 = map(test, x => x*2)
      assert(forall(mapp, x => x==1))
      assert(contains(mapp2,2))
      assert(contains(mapp2,4))
      assert(contains(mapp2,6))
      assert(!contains(mapp2,3))
    }
  }
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
