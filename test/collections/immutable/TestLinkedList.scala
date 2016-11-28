package linovi.collections.immutable
import org.specs2.mutable._

/**
  * Created by mersanuzun on 11/15/16.
  */
class TestLinkedList extends Specification{
  "Immutable LinkedList" should {
    "add an element to empty list" in {
      val list: LinkedList[Int] = LinkedList()
      val newList = list.add(1)
      newList.head mustEqual 1
      newList.tail mustEqual EmptyNode
    }

    "add an element to non-empty list that is constructed with apply method" in {
      val list: LinkedList[String] = LinkedList("Mehmet", "Ersan")
      val newList = list.add("Uzun")
      newList.head mustEqual "Uzun"
      newList.tail.head mustEqual "Mehmet"
      newList.tail.tail.head mustEqual "Ersan"
      newList.tail.tail.tail mustEqual EmptyNode
    }

    "throw exception when head is called in empty list" in {
      val list: LinkedList[Int] = LinkedList()
      list.head must throwA (new NoSuchElementException)
    }

    "throw exception when tail is called in empty list" in {
      val list: LinkedList[Int] = LinkedList()
      list.tail must throwA (new NoSuchElementException)
    }

    "remove an element from empty list" in {
      val list: LinkedList[Int] = LinkedList()
      list.remove(1) mustEqual EmptyNode
    }

    "remove an element that is added before" in {
      val list: LinkedList[Int] = LinkedList(1,2,3)
      val newList = list.remove(2)
      newList.head mustEqual 1
      newList.tail.head mustEqual 3
      newList.tail.tail mustEqual EmptyNode
    }

    "return true invoking isEmpty method in empty list" in {
      val list: LinkedList[Int] = LinkedList()
      list.isEmpty mustEqual true
    }

    "return false invoking isEmpty method in non-empty list" in {
      val list: LinkedList[Int] = LinkedList(1,2,3)
      list.isEmpty mustEqual false
    }

    "return true invoking contains with an element that is added before" in {
      val list: LinkedList[String] = LinkedList("Mehmet", "Ersan", "Uzun")
      list.contains("Ersan") mustEqual true
    }

    "return false invoking contains with an element that is not added before" in {
      val list: LinkedList[String] = LinkedList("Mehmet", "Ersan", "Uzun")
      list.contains("Erkan") mustEqual false
    }


    "adding 1000000 elements" in {
      var list: LinkedList[Int] = LinkedList()
      for(i <- 1 to 1000000){
        list = list.add(i)
      }
      list.remove(50000)
      "added" equals "added"
    }

  }
}
