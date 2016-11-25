package linovi.collections.mutable


/**
  * Created by mersanuzun on 10/18/16.
  */
class HashMap[K, V] {
  private val bucketInitCapacity: Int = 16
  private[mutable] val bucket: Array[MapEntry] = Array.fill(bucketInitCapacity)(EmptyMapEntry)

  private def hash(hashCode: Int): Int = {
    hashCode ^ (hashCode >>> 20) ^ (hashCode >>> 12)
  }

  private[mutable] def getIndex(key: K): Int = {
    val h: Int = hash(key.hashCode)
    indexFor(h, bucket.length)
  }

  def contains(key: K): Boolean = bucket(getIndex(key)).contains(key)

  private def indexFor(h: Int, length: Int): Int = h & length -1

  def put(key: K, value: V): Option[V] = {
    val index: Int = getIndex(key)
    var currentNode = bucket(index)
    if (currentNode == EmptyMapEntry){
      bucket(index) = NonEmptyMapEntry(key, value, EmptyMapEntry)
    } else {
      while(currentNode.next != EmptyMapEntry || currentNode.key.equals(key)){
        if (currentNode.key.equals(key)){
          val tempValue = currentNode.value
          currentNode.asInstanceOf[NonEmptyMapEntry].value = value
          return Some(tempValue)
        }
        currentNode = currentNode.next
      }
      currentNode.asInstanceOf[NonEmptyMapEntry].next = NonEmptyMapEntry(key, value, EmptyMapEntry)
    }
    None
  }

  def remove(key: K): Option[V] = {
    val index: Int = getIndex(key)
    var currentNode: MapEntry = bucket(index)
    var prev: MapEntry = bucket(index)
    if (currentNode == EmptyMapEntry) return None
    if (currentNode.key == key) {
      bucket(index) = EmptyMapEntry
      return Some(currentNode.value)
    }
    while(currentNode != EmptyMapEntry && currentNode.key != key){
      prev = currentNode
      currentNode = currentNode.next
    }
    if (currentNode == EmptyMapEntry) return None
    prev.asInstanceOf[NonEmptyMapEntry].next = currentNode.next
    Some(currentNode.value)
  }

  private[mutable] trait MapEntry{
    def key: K
    def value: V
    def next: MapEntry
    def contains(k: K): Boolean
  }
  private[mutable] case class NonEmptyMapEntry(key: K, var value: V, var next: MapEntry) extends MapEntry {
    override def contains(k: K): Boolean = {
      if (key.equals(k)) true
      else next.contains(k)
    }
  }
  private[mutable] object EmptyMapEntry extends MapEntry{
    override def key: K = throw new NoSuchElementException
    override def value: V = throw new NoSuchElementException
    override def next: MapEntry = throw new NoSuchElementException
    override def contains(k: K): Boolean = false
    override def toString: String = "-"
  }
}