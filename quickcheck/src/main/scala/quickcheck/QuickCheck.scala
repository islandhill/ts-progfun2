package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- frequency((1, const(empty)), (9, genHeap))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //should get the smallest for the elements by inserting them into an empty heap
  property("InsTwoIntoEmpty") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(x, empty))
    val smallest = if (x > y) y else x
    findMin(heap) == smallest
  }

  //The resulting heap should be empty if an element is inserted then deleted as the min
  property("insDelMin") = forAll { (x: Int) =>
    val heap = insert(x, empty)
    deleteMin(heap) == empty
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("melbMin") = forAll { (h1: H, h2: H) =>
    val meldResult = meld(h1, h2)
    val minElement = findMin(meldResult)
    minElement == findMin(h1) || minElement == findMin(h2)
  }

  def recordMin(original: H, accumulator: List[Int]): List[Int] = {
    if (isEmpty(original)) accumulator
    else findMin(original) :: recordMin(deleteMin(original), accumulator)
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("recordMinAsSorted") = forAll { (h1: H) =>
    val listAssumedSorted = recordMin(h1, Nil)
    listAssumedSorted == listAssumedSorted.sorted
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    recordMin(meld1, Nil) == recordMin(meld2, Nil)
  }

}
