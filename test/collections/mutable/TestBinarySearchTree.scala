package linovi.collections.mutable
import org.specs2.mutable._

/**
  * Created by mersanuzun on 11/14/16.
  */
class TestBinarySearchTree extends Specification{
  "Mutable Binary Search Tree" should {
    "add an element into an empty BST" in {
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      bst.add(1)
      bst.root.value mustEqual 1
    }

    "add an element greater than parent" in {
      val bst: BinarySearchTree[String] = new BinarySearchTree[String]
      bst.add("d")
      bst.add("c")
      bst.add("e")
      bst.root.rightChild.value mustEqual "e"
    }

    "add an element less than parent" in {
      val bst: BinarySearchTree[Double] = new BinarySearchTree[Double]
      bst.add(3.0)
      bst.add(4.2)
      bst.add(2)
      bst.add(2.9)
      bst.add(1.2)
      bst.root.leftChild.leftChild.value mustEqual 1.2
    }

    "remove an element that right child is empty" in {
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      bst.add(1)
      bst.add(0)
      bst.add(5)
      bst.add(3)
      bst.add(2)
      bst.remove(5)
      bst.root.rightChild.value mustNotEqual 5
      bst.root.rightChild.value mustEqual 3
      bst.root.rightChild.leftChild.value mustEqual 2
    }

    "remove an element that right child is not empty" in {
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      bst.add(5)
      bst.add(4)
      bst.add(6)
      bst.add(3)
      bst.add(7)
      bst.add(6)
      bst.add(6)
      bst.remove(6)
      print(bst.root)
      bst.root.rightChild.value mustEqual 6
      bst.root.rightChild.rightChild.value mustEqual 7
    }

    "return true invoking contains with an element that is not added before" in {
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      bst.add(1)
      bst.add(0)
      bst.add(3)
      bst.contains(0) mustEqual true
      bst.contains(3) mustEqual true
      bst.contains(1) mustEqual true
    }

    "return false invoking contains with an element that is not added before" in {
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      bst.add(1)
      bst.add(0)
      bst.add(3)
      bst.contains(9) mustEqual false
    }


    "adding 100000 elements" in {
      val rnd = scala.util.Random
      val bst: BinarySearchTree[Int] = new BinarySearchTree[Int]
      for(i <- 1 to 10000000){
        bst.add(rnd.nextInt(10000))
      }
      "added" equals "added"
    }
  }
}

/*
     1
   0   5
     3   8
    2 4 7 9
 */



/*     5
    4    6
  3        7
          6
            6
*/