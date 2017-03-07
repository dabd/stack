package stack

object StackModule {

  case class Stack[A](items: List[A]) {

    // Basic operations

    def push(a: A): Stack[A] = Stack(a :: items)

    def pop: (Option[A], Stack[A]) =
      if (this.isEmpty) (None, Stack(Nil))
      else (Some(items.head), Stack(items.tail))

    def peek: Option[A] = if (this.isEmpty) None else Some(items.head)

    def isEmpty: Boolean = items.isEmpty

    // Extra operations

    // duplicate the top element
    def duplicate: Stack[A] =
      if (this.isEmpty) this
      else this.push(this.peek.get)

    // swap top two items
    def swap: Stack[A] = items match {
      case Nil => Stack(Nil)
      case x :: Nil => Stack(List(x))
      case x :: y :: zs => Stack(y :: x :: zs)
    }

    // rotate the topmost n elements so the topmost element becomes the nth from the top,
    // the second from the top becomes the top, etc.
    def rotate(n: Int): Stack[A] = {
      if (n > items.size) this
      else {
        val (topmostN, rem) = items.splitAt(n)
        topmostN match {
          case Nil => this
          case h :: t => Stack(t ::: List(h) ::: rem)
        }
      }
    }

    // pop n elements from the stack
    def popN(n: Int): (List[A], Stack[A]) =
      (items.take(n), Stack(items.drop(n)))

  }

}
