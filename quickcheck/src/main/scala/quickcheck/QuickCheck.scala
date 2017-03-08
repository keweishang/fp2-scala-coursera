package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    item <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(item, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallerBetween2") = forAll { (a : Int, b : Int) =>
    val small = if (a < b) a else b
    val large = if (a < b) b else a
    val h1 = insert(small, empty)
    val h2 = insert(large, h1)
    val min = findMin(h2)
    min == small
  }

  property("shouldBeEmpty") = forAll { a : Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("sorted") = forAll { (h : H) =>
    def items(cur: H) : List[Int] = {
      if (isEmpty(cur)) List.empty
      else {
        findMin(cur) :: items(deleteMin(cur))
      }
    }

    val list = items(h)
    ("list = " + list) |: all(
      list == list.sorted
    )
  }

  property("minInMeld") = forAll { (h1 : H, h2 : H) =>
    val h = meld(h1, h2)
    findMin(h) == Math.min(findMin(h1), findMin(h2))
  }

  property("unchangedMeld") = forAll { (h1 : H, h2 : H) =>
    def items(cur: H) : List[Int] = {
      if (isEmpty(cur)) List.empty
      else {
        findMin(cur) :: items(deleteMin(cur))
      }
    }

    val m1 = meld(h1, h2)
    val min = findMin(h1)
    val m2 = meld(deleteMin(h1), insert(min, h2))
    val items1 = items(m1)
    val items2 = items(m2)
    items1 == items2
  }

}
