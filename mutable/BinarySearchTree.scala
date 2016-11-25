package linovi.collections.mutable

import scala.collection.mutable

/**
  * Created by mersanuzun on 10/17/16.
  */
class BinarySearchTree[T <% Ordered[T]] {
  private[mutable] var root: Node = EmptyNode

  def add(value: T): Unit = root = root.add(value)

  def contains(value: T): Boolean = root.contains(value)

  def remove(value: T): Unit = root = root.remove(value)

  trait Node{
    def value: T
    def leftChild: Node
    def rightChild: Node
    def add(v: T): Node
    def remove(v: T): Node
    def isEmpty: Boolean
    def contains(v: T): Boolean
  }
  case class NonEmptyNode(var value: T, var leftChild: Node, var rightChild: Node) extends Node {
    override def add(v: T): Node = {
      if (v < value){
        NonEmptyNode(value, leftChild.add(v), rightChild)
      }else{
        NonEmptyNode(value, leftChild, rightChild.add(v))
      }
    }
    override def isEmpty: Boolean = false
    override def contains(v: T): Boolean = {
      if (v == value) true
      else if (v > value) rightChild.contains(v)
      else leftChild.contains(v)
    }
    override def remove(v: T): Node = {
      if (v < value){
        NonEmptyNode(value, leftChild.remove(v), rightChild)
      }else if (v > value){
        NonEmptyNode(value, leftChild, rightChild.remove(v))
      }else{
        if (rightChild != EmptyNode) {
          val deletedNode: Node = findMinimum(rightChild)
          NonEmptyNode(deletedNode.value, leftChild, rightChild.remove(deletedNode.value))
        }
        else if (leftChild != EmptyNode) leftChild
        else EmptyNode
      }

    }
    private def findMinimum(child: Node): Node = {
      child.leftChild match {
        case EmptyNode => child
        case n: NonEmptyNode => findMinimum(n)
      }
    }
  }
  object EmptyNode extends Node {
    def value: T = throw new NoSuchElementException
    def leftChild: Node = throw new NoSuchElementException
    def rightChild: Node = throw new NoSuchElementException
    override def toString: String = "-"
    override def add(v: T): Node = NonEmptyNode(v, EmptyNode, EmptyNode)
    override def isEmpty: Boolean = true
    override def contains(v: T): Boolean = false
    override def remove(v: T): Node = EmptyNode
  }
}