package linovi.collections.mutable

import org.specs2.mutable._
/**
  * Created by mersanuzun on 11/15/16.
  */
class TestHashMap extends Specification{
List
  "Mutable HashMap" should {
    "put data that its key is 1 to empty hashmap" in {
      val hashmap: HashMap[Int, String] = new HashMap[Int, String]
      hashmap.put(1, "Ersan") mustEqual None
      hashmap.bucket(1).key mustEqual 1
      hashmap.bucket(1).value mustEqual "Ersan"
    }

    "put two data that is different keys" in {
      val hashmap: HashMap[Int, String] = new HashMap[Int, String]
      hashmap.put(1, "Ersan") mustEqual None
      hashmap.put(2, "Mehmet") mustEqual None
      hashmap.bucket(1).value mustEqual "Ersan"
      hashmap.bucket(2).value mustEqual "Mehmet"
    }

    "put two same key data" in {
      val hashMap: HashMap[String, String] = new HashMap[String, String]
      hashMap.put("one", "bir") mustEqual None
      hashMap.put("one", "eins") mustEqual Some("bir")
      hashMap.bucket(12).value mustEqual "eins"
    }

    "put more than one same indexed key data" in {
      val hashMap: HashMap[Int, String] = new HashMap[Int, String]
      hashMap.put(1, "bir") mustEqual None
      hashMap.put(17, "on yedi") mustEqual None
      hashMap.put(33, "otuz üç") mustEqual None
      hashMap.bucket(1).value mustEqual "bir"
      hashMap.bucket(1).next.value mustEqual "on yedi"
      hashMap.bucket(1).next.next.value mustEqual "otuz üç"
    }

    "remove an element from empty hash map" in {
      val hashMap: HashMap[Int, String] = new HashMap[Int, String]
      hashMap.remove(2) mustEqual None
    }

    "remove non-added element" in {
      val hashMap: HashMap[Int, String] = new HashMap[Int, String]
      hashMap.put(1, "One") mustEqual None
      hashMap.remove(2) mustEqual None
      hashMap.bucket(1).value mustEqual "One"
    }

    "remove an added element" in {
      val hashMap: HashMap[Int, String] = new HashMap[Int, String]
      hashMap.put(1, "One") mustEqual None
      hashMap.remove(1) mustEqual Some("One")
      hashMap.bucket(1) mustEqual hashMap.EmptyMapEntry
    }

    "remove a element from same indexed data" in {
      val hashMap: HashMap[Int, String] = new HashMap[Int, String]
      hashMap.put(1, "bir") mustEqual None
      hashMap.put(17, "on yedi") mustEqual None
      hashMap.put(33, "otuz üç") mustEqual None
      hashMap.remove(17)
      hashMap.bucket(1).value mustEqual "bir"
      hashMap.bucket(1).next.value mustEqual "otuz üç"
    }

    "return true if an added element is checked" in {
      val hashMap: HashMap[Int, Double] = new HashMap[Int, Double]
      hashMap.put(1, 1.0) mustEqual None
      hashMap.put(17, 17.0) mustEqual None
      hashMap.contains(1) mustEqual true
      hashMap.contains(17) mustEqual true
    }

    "return false if a non-added element is checked" in {
      val hashMap: HashMap[Int, Double] = new HashMap[Int, Double]
      hashMap.put(17, 17.0) mustEqual None
      hashMap.contains(1) mustEqual false
    }

    "adding 100000 elements" in {
      val hashMap: HashMap[Int, Double] = new HashMap[Int, Double]
      for(i <- 1 to 100000){
        hashMap.put(i, i)
      }
      "added" equals "added"
    }
  }

}
