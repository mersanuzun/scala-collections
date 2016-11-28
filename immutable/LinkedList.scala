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
  def removeFist(): LinkedList[T] = tail
  def drop(n: Int): LinkedList[T] = {
    if (n == 0) this
    else tail.drop(n - 1)
  }
  def dropWhile(f: T => Boolean): LinkedList[T]
  def take(n: Int): LinkedList[T]
  def setHead[B >: T](v: B): LinkedList[B]
  def init(): LinkedList[T] = {
    if (tail == EmptyNode) EmptyNode
    else NonEmptyNode(head, tail.init())
  }
  def foldLeft[B](x: B)(f: (T, B) => B): B
  def foldRight[B](x: B)(f: (T, B) => B): B
  def length: Int = foldLeft(0)((x, y) => y + 1)
  def concat[B >: T](list: LinkedList[B]): LinkedList[B] = foldRight(list)((x, xs) => xs.add(x))
  def append[B >: T](e: B): LinkedList[B] = foldRight(LinkedList(e))((x, xs) => xs.add(x))
  def hasSubsequence[B >: T](xs: LinkedList[B]): Boolean
}

case class NonEmptyNode[T](override val head: T, override val tail: LinkedList[T]) extends LinkedList[T]{
  override def isEmpty: Boolean = false

  override def remove[B >: T](value: B): LinkedList[B] = {
    if (value == this.head) tail
    else NonEmptyNode(this.head, tail.remove(value))
  }

  override def dropWhile(f: (T) => Boolean): LinkedList[T] = {
    if (f(head)) tail.dropWhile(f)
    else NonEmptyNode(head, tail.dropWhile(f))
  }

  override def setHead[B >: T](v: B): LinkedList[B] = NonEmptyNode(v, tail)

  override def foldLeft[B](x: B)(f: (T, B) => B): B = tail.foldLeft(f(head, x))(f)

  override def foldRight[B](x: B)(f: (T, B) => B): B = f(head, tail.foldRight(x)(f))

  override def hasSubsequence[B >: T](xs: LinkedList[B]): Boolean = {
    if (take(xs.length).equals(xs)) true
    else tail.hasSubsequence(xs)
  }

  override def take(n: Int): LinkedList[T] = {
    if (n == 0) EmptyNode
    else NonEmptyNode(head, tail.take(n - 1))
  }
}
object EmptyNode extends LinkedList[Nothing] {
  override def isEmpty = true
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def remove[B >: Nothing](value: B): LinkedList[B] = EmptyNode
  override def add[B >: Nothing](value: B): LinkedList[B] = NonEmptyNode(value, EmptyNode)
  override def dropWhile(f: (Nothing) => Boolean): LinkedList[Nothing] = EmptyNode
  override def setHead[B >: Nothing](v: B): LinkedList[B] = EmptyNode
  override def foldLeft[B](x: B)(f: (Nothing, B) => B): B = x
  override def foldRight[B](x: B)(f: (Nothing, B) => B): B = x
  override def hasSubsequence[B >: Nothing](xs: LinkedList[B]): Boolean = false
  override def take(n: Int): LinkedList[Nothing] = EmptyNode
}

object LinkedList {
  def apply[T](xs: T*): LinkedList[T] = {
    if(xs.isEmpty) EmptyNode
    else NonEmptyNode(xs.head, apply(xs.tail: _*))
  }
}