package stack

import org.scalacheck._
import stack.StackModule._

class StackModuleTest extends CommonSpec {

  def genStack(maxSize: Option[Int] = None): Gen[Stack[Int]] =
    for {
      items <- maxSize match {
        case None => Gen.listOf(Arbitrary.arbInt.arbitrary)
        case Some(_) =>
          Gen.choose(0, maxSize.get) flatMap { sz =>
            Gen.listOfN(sz, Arbitrary.arbInt.arbitrary)
          }
      }
    } yield Stack(items)

  "push" should {
    "add element to the top of the stack" in {
      forAll(genStack(), Arbitrary.arbInt.arbitrary) {
        case (s, e) =>
          s.push(e).peek shouldBe Some(e)
      }
    }
  }

  "pop" should {
    "return None and an empty stack on an empty stack" in {
      Stack(Nil).pop shouldBe (None, Stack(Nil))
    }

    "return the top of the stack and the stack minus the element on a non-empty stack" in {
      forAll(genStack(), Arbitrary.arbInt.arbitrary) {
        case (s, e) =>
          s.push(e).pop shouldBe (Some(e), s)
      }
    }
  }

  "peek" should {
    "return None on an empty stack" in {
      Stack(Nil).peek shouldBe None
    }

    "return the top of the stack on a non-empty stack" in {
      forAll(genStack(), Arbitrary.arbInt.arbitrary) {
        case (s, e) =>
          s.push(e).peek shouldBe Some(e)
      }
    }
  }

  "isEmpty" should {
    "return true on an empty stack" in {
      Stack(Nil).isEmpty shouldBe true
    }

    "return false on a non-empty stack" in {
      forAll(genStack(), Arbitrary.arbInt.arbitrary) {
        case (s, e) =>
          s.push(e).isEmpty shouldBe false
      }
    }
  }

  "duplicate" should {
    "return an empty stack if it is initially empty" in {
      Stack(Nil).duplicate shouldBe Stack(Nil)
    }

    "duplicate the topmost element of the stack" in {
      forAll(genStack(), Arbitrary.arbInt.arbitrary) {
        case (s, e) =>
          s.push(e).duplicate shouldBe s.push(e).push(e)
      }
    }
  }

  "swap" should {
    "return the stack unchanges if it is has less than 2 elements" in {
      Stack(Nil).swap shouldBe Stack(Nil)
      Stack(List(1)).swap shouldBe Stack(List(1))
    }

    "swap the top 2 elements of the stack" in {
      forAll(genStack(),
             Arbitrary.arbInt.arbitrary,
             Arbitrary.arbInt.arbitrary) {
        case (s, e1, e2) =>
          s.push(e1).push(e2).swap shouldBe s.push(e2).push(e1)
      }
    }
  }

  "popN n" should {
    "return the topmost n elements form the stack" in {
      forAll(for {
        n <- Gen.choose(0, 5)
        s <- genStack(Some(n + 10)) suchThat (_.items.size >= n)
      } yield (n, s)) {
        case (n, s) =>
          s.popN(n) shouldBe {
            val (topN, rem) = s.items.splitAt(n)
            (topN, Stack(rem))
          }
      }
    }
  }

  "rotate n" should {

    """should move up one position all the n-1 elements below the top and
        |move the top element to the bottom of the n-1 elements below it
      """.stripMargin in {
      forAll(for {
        s <- genStack(Some(50))
        n <- Gen
          .choose(0, s.items.size)
      } yield (s, n)) {
        case (s, n) =>
          (s.items, n) match {
            case (_, 0) => s.rotate(0) shouldBe s
            case (Nil, n) => s.rotate(n) shouldBe s
            case (h :: t, n) => {
              val rotated = s.rotate(n).items
              rotated.take(n - 1) shouldBe t.take(n - 1)
              rotated(n - 1) shouldBe h
            }
          }

      }
    }

    "rotate the topmost n elements of the stack at most the size of the stack times should return the original stack" in {
      forAll(for {
        s <- genStack(Some(50))
        n <- Gen
          .choose(0, s.items.size)
      } yield (s, n)) {
        case (s, n) =>
          (1 to (n min s.items.size)).foldLeft(s: Stack[Int]) {
            case (s, _) => s.rotate(n)
          } shouldBe s
      }
    }

    "rotate n only reorders the topmost n elements of the stack" in {
      forAll(for {
        s <- genStack(Some(50))
        n <- Gen
          .choose(0, s.items.size)
      } yield (s, n)) {
        case (s, n) =>
          val (_, s2) = s.rotate(n).popN(n)
          val (_, s3) = s.popN(n)

          s2 shouldBe s3
      }
    }

  }
}
