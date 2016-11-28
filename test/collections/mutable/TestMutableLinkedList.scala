package linovi.collections.mutable

import org.specs2.mutable._
/**
  * Created by mersanuzun on 11/8/16.
  */
class TestMutableLinkedList extends Specification{

  "Mutable LinkedList" should {
    "add an element into the end" in {
      val list: LinkedList[Int] = new LinkedList[Int]
      list.addLast(2)
      list.addLast(1)
      println(list)
      list.firstNode.value mustEqual 2
      list.firstNode.next.value mustEqual(1)
    }

    "add an element into head" in {
      val list: LinkedList[String] = new LinkedList[String]
      list.addFirst("Uzun")
      list.addFirst("Ersan")
      list.firstNode.value mustEqual "Ersan"
      list.firstNode.next.value mustEqual "Uzun"
    }

    "remove a giving element" in {
      val list: LinkedList[Double] = new LinkedList[Double]
      list.addFirst(1.2)
      list.addFirst(25.2)
      list.addFirst(2.3)
      list.remove(25.2)
      list.firstNode.value mustEqual 2.3
      list.firstNode.next.value mustEqual 1.2
    }

    "return true contains method when searching an added element" in {
      val list: LinkedList[Int] = new LinkedList[Int]
      list.addLast(1)
      list.addLast(2)
      list.addLast(3)
      list.firstNode.value mustEqual 1
      list.firstNode.next.value mustEqual 2
      list.firstNode.next.next.value mustEqual 3
    }

    "return false contains method when searching an non-added element" in {
      val list: LinkedList[Int] = new LinkedList[Int]
      list.addLast(1)
      list.firstNode.value mustNotEqual 2
    }

    "add 1 million value into front" in {
      val list: LinkedList[Int] = new LinkedList[Int]
      for(i <- 1 to 1000000){
        list.addFirst(i)
      }
      true
    }
  }

}


