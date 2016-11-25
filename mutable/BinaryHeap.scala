package linovi.collections.mutable

import scala.collection.mutable.ArrayBuffer

/**
  * Created by mersanuzun on 10/24/16.
  */
class BinaryHeap[T <% Ordered[T]] {
  private[mutable] val heap: ArrayBuffer[T] = ArrayBuffer[T]()

  private def parentIndex(k: Int): Int = (k - 1) / 2

  private def swap(index: Int, pIndex: Int) = {
    val tempValue: T = heap(pIndex)
    heap(pIndex) = heap(index)
    heap(index) = tempValue
  }

  def add(value: T): BinaryHeap[T] = {
    heap += value
    upHeap()
    this
  }

  def contains(value: Int): Boolean = heap.contains(value)

  private def upHeap(): Unit = {
    var index: Int = heap.length - 1
    var pIndex: Int = parentIndex(index)
    while (heap(index) < heap(pIndex)){
      swap(index, pIndex)
      index = pIndex
      pIndex = parentIndex(index)
    }
  }

  def removeSmallest(): T = {
    if (heap.nonEmpty){
      val removed: T = heap(0)
      if (heap.length > 1){
        heap(0) = heap.remove(heap.length - 1)
      }else {
        heap.remove(heap.length - 1)
      }
      downHeap()
      removed
    }else{
      throw new NoSuchElementException("Empty Heap")
    }
  }

  private def minChild(index: Int): Int = {
    if ((index * 2) + 2 >= heap.length){
      (index * 2) + 1
    }else {
      if (heap((index * 2) + 1) < heap((index * 2) + 2)){
        (index * 2) + 1
      }else{
        (index * 2) + 2
      }
    }
  }

  private def downHeap(): Unit = {
    var index: Int = 0
    while((index * 2) + 1 < heap.length){
      val min: Int = minChild(index)
      if (heap(index) > heap(min)){
        swap(index, min)
      }
      index = min
    }
  }

  def foreach(f: T => Unit): Unit = {
    heap.foreach(f)
  }

  def filter(f: T => Boolean): BinaryHeap[T] = {
    val newHeap = new BinaryHeap[T]
    for{
      value <- heap
      if f(value)
    } newHeap.add(value)
    newHeap
  }

  def map[B <% Ordered[B]](f: T => B): BinaryHeap[B] = {
    val newHeap = new BinaryHeap[B]
    for(value <- heap) {
      newHeap.add(f(value))
    }
    newHeap
  }
}