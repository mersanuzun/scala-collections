package linovi.collections.immutable

/**
  * Created by mersanuzun on 10/17/16.
  */

trait LinkedList[+T] {
  def add[B >: T](value: B): LinkedList[B] = NonEmptyNode(value, this)
  def head: T
  def tail: LinkedList[T]
  def isEmpty: Boolean
  def contains[B >: T](value: B): Boolean = {
    var currNode: LinkedList[B] = this
    while(!currNode.isEmpty){
      if (currNode.head == value) return true
      currNode = currNode.tail
    }
    false
  }
  def remove[B >: T](value: B): LinkedList[B]
}

case class NonEmptyNode[T](override val head: T, override val tail: LinkedList[T]) extends LinkedList[T]{
  override def isEmpty: Boolean = false

  override def remove[B >: T](value: B): LinkedList[B] = {
    if (value == this.head) tail
    else NonEmptyNode(this.head, tail.remove(value))
  }
}
object EmptyNode extends LinkedList[Nothing] {
  override def isEmpty = true
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def remove[B >: Nothing](value: B): LinkedList[B] = EmptyNode
  override def add[B >: Nothing](value: B): LinkedList[B] = NonEmptyNode(value, EmptyNode)
}

object LinkedList {
  def apply[T](xs: T*): LinkedList[T] = {
    if(xs.isEmpty) EmptyNode
    else NonEmptyNode(xs.head, apply(xs.tail: _*))
  }
}