package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.{min}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property(
    """
    If you insert any two elements into an empty heap, 
    finding the minimum of the resulting heap 
    should get the smallest of the two elements back."""
  ) =
    forAll { (v1: Int, v2: Int) =>
      val h1 = insert(v1, empty)
      val h2 = insert(v2, h1)
      findMin(h2) == min(v1, v2)
    }

  property(
    """If you insert an element into an empty heap, 
    then delete the minimum, the resulting heap should be empty."""
  ) = forAll { (i: Int) =>
    val h = insert(i, empty)
    isEmpty(deleteMin(h)) == true
  }

  property(
    """Given any heap, you should get a sorted sequence of elements 
    when continually finding and deleting minimal. 
    (Hint: recursion and helper functions are your friends.)"""
  ) = forAll {(h: H)=>
    def isSorted (h: H, prev: Int): Boolean = {
      isEmpty(h) || {
        val minEl = findMin(h)
        minEl >= prev && isSorted(deleteMin(h), minEl)
      }
    }
    isSorted(h, findMin(h))
  }

  property(
    """Finding a minimum of the melding of any two heaps 
    should return a minimum of one or the other."""
  ) = forAll {(h1: H, h2: H)=>
    findMin(
      meld(h1, h2)
    ) == min(
      findMin(h1),
      findMin(h2)
    )
  }

  property(
    "melding and inserting doesn't change the content"
  ) = forAll{(h1: H, h2: H) =>
    def heapsEqual (h1: H, h2: H): Boolean =
      isEmpty(h1) && isEmpty(h2) || (
        findMin(h1) == findMin(h2) && heapsEqual(deleteMin(h1), deleteMin(h2))
      )
    heapsEqual(
      meld(deleteMin(h1), insert(findMin(h1), h2)),
      meld(h1, h2)
    )
  }
}
