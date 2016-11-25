package linovi.collections.mutable

/**
  * Created by mersanuzun on 10/17/16.
  */
class LinkedList[T] {
  private[mutable] var firstNode: Node = EmptyNode

  def addLast(data: T) = {
    firstNode = firstNode.addLast(data)
  }

  def addFirst(data: T) = {
    firstNode = NonEmptyNode(data, firstNode)
  }

  def remove(value: T) = {
    firstNode = firstNode.remove(value)
  }

  def contains(value: T): Boolean = {
    firstNode.contains(value)
  }

  def foreach(f: T => Unit): Unit = {
    var currentNode: Node = firstNode
    while(currentNode != EmptyNode){
      f(currentNode.value)
      currentNode = currentNode.next
    }
  }

  /*def filter(f: T => Boolean): LinkedList[T] = {
    val linked: LinkedList[T] = new LinkedList[T]
    var currentNode: Node = first
    while (currentNode != EmptyNode){
      if (f(currentNode.value)) linked.addLast(currentNode.value)
      currentNode = currentNode.next
    }
    linked
  }

  override def toString: String = {
    var str: String = "LinkedList("
    var currentNode: Node = first
    while(currentNode != EmptyNode){
      str += currentNode.value + ", "
      currentNode = currentNode.next
    }
    str.substring(0, str.length - 2) + ")"
  }*/

  private[mutable] trait Node{
    def value: T
    def next: Node
    def addLast(value: T): Node
    def isEmpty: Boolean
    def remove(v: T): Node
    def contains(v: T): Boolean
  }
  private[mutable] case class NonEmptyNode(value: T, var next: Node) extends Node {
    override def addLast(value: T): Node = NonEmptyNode(this.value, next.addLast(value))
    override def isEmpty: Boolean = false
    override def remove(v: T): Node = {
      if (v == value) next
      else NonEmptyNode(value, next.remove(v))
    }
    override def contains(v: T): Boolean = {
      if (v == value) true
      else next.contains(v)
    }
  }
  private[mutable] object EmptyNode extends Node {
    override def value = throw new NoSuchElementException
    override def next = throw new NoSuchElementException
    override def toString: String = "-"
    override def addLast(value: T): Node = NonEmptyNode(value, EmptyNode)
    override def isEmpty: Boolean = true
    override def remove(v: T): Node = EmptyNode
    override def contains(v: T): Boolean = false
  }
}