package linovi.collections.mutable
import linovi.collections.immutable.TestLinkedList
import org.specs2.mutable._

import scala.collection.mutable

/**
  * Created by mersanuzun on 11/14/16.
  */
class TestBinaryHeap extends Specification{
  "Mutable Binary Heap" should {
    "add an element into empty heap" in {
      val binaryHeap: BinaryHeap[Int] = new BinaryHeap[Int]
      binaryHeap.add(1)
      binaryHeap.heap.head mustEqual 1
    }

    "add 0 into [1,2,4]" in {
      val binaryHeap: BinaryHeap[Int] = new BinaryHeap[Int]
      binaryHeap.add(1)
      binaryHeap.add(2)
      binaryHeap.add(4)
      binaryHeap.add(0)
      binaryHeap.heap.head mustEqual 0
      binaryHeap.heap(1) mustEqual 1
      binaryHeap.heap(2) mustEqual 4
      binaryHeap.heap(3) mustEqual 2
    }

    "add 5 into [1,2,4]" in {
      val binaryHeap: BinaryHeap[Int] = new BinaryHeap[Int]
      binaryHeap.add(1)
      binaryHeap.add(2)
      binaryHeap.add(4)
      binaryHeap.add(5)
      binaryHeap.heap.head mustEqual 1
      binaryHeap.heap(1) mustEqual 2
      binaryHeap.heap(2) mustEqual 4
      binaryHeap.heap(3) mustEqual 5
    }

    "remove smallest element" in {
      val binaryHeap: BinaryHeap[String] = new BinaryHeap[String]
      binaryHeap.add("a")
      binaryHeap.add("b")
      binaryHeap.add("d")
      binaryHeap.add("c")
      binaryHeap.removeSmallest()
      binaryHeap.heap.head mustEqual "b"
      binaryHeap.heap(1) mustEqual "c"
      binaryHeap.heap(2) mustEqual "d"
    }

    "remove smallest element from an empty heap" in {
      val binaryHeap: BinaryHeap[String] = new BinaryHeap[String]
      binaryHeap.removeSmallest() must throwA(new NoSuchElementException("Empty Heap"))
    }
  }

}
