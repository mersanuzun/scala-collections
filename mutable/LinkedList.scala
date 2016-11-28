package linovi.collections.mutable

/**
  * Created by mersanuzun on 10/17/16.
  */
class LinkedList[T] {
  private[mutable] var firstNode: Node = EmptyNode

  def addLast(data: T): LinkedList[T] = {
    var currentNode: Node = firstNode
    if (firstNode == EmptyNode){
      firstNode = NonEmptyNode(data, EmptyNode)
      return this
    }
    while(currentNode.next != EmptyNode){
      currentNode = currentNode.next
    }
    currentNode.asInstanceOf[NonEmptyNode].next = NonEmptyNode(data, EmptyNode)
    this
  }

  def addFirst(data: T) = {
    firstNode = NonEmptyNode(data, firstNode)
  }

  def remove(value: T): Boolean = {
    var currentNode: Node = firstNode
    var prev: Node = firstNode
    if (firstNode.value == value) {
      firstNode = currentNode.next
      return true
    }
    while(currentNode != EmptyNode && currentNode.value != value){
      prev = currentNode
      currentNode = currentNode.next
    }
    if (currentNode == EmptyNode) return false
    prev.asInstanceOf[NonEmptyNode].next = currentNode.next
    true
  }

  def contains(value: T): Boolean = {
    var currentNode: Node = firstNode
    while(currentNode != EmptyNode){
      if (currentNode.value == value) return true
      currentNode = currentNode.next
    }
    false
  }

  def foreach(f: T => Unit): Unit = {
    var currentNode: Node = firstNode
    while(currentNode != EmptyNode){
      f(currentNode.value)
      currentNode = currentNode.next
    }
  }

  def filter(f: T => Boolean): LinkedList[T] = {
    val linked: LinkedList[T] = new LinkedList[T]
    var currentNode: Node = firstNode
    while (currentNode != EmptyNode){
      if (f(currentNode.value)) linked.addLast(currentNode.value)
      currentNode = currentNode.next
    }
    linked
  }

  override def toString: String = {
    var str: String = "LinkedList("
    var currentNode: Node = firstNode
    while(currentNode != EmptyNode){
      str += currentNode.value + ", "
      currentNode = currentNode.next
    }
    str.substring(0, str.length - 2) + ")"
  }

  private[mutable] trait Node{
    def value: T
    def next: Node
    def isEmpty: Boolean
  }
  private[mutable] case class NonEmptyNode(value: T, var next: Node) extends Node {
    override def isEmpty: Boolean = false
  }
  private[mutable] object EmptyNode extends Node {
    override def value = throw new NoSuchElementException
    override def next = throw new NoSuchElementException
    override def toString: String = "-"
    override def isEmpty: Boolean = true
  }
}