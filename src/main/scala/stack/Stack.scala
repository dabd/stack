package stack

case class Stack[A](items: List[A]) {

  // Basic operations

  def push(a: A): Stack[A] = Stack(a :: items)

  def pop: (Option[A], Stack[A]) = items match {
    case Nil => (None, Stack(Nil))
    case h :: t => (Some(h), Stack(t))
  }

  def peek: Option[A] = items match {
    case Nil => None
    case h :: _ => Some(h)
  }

  def isEmpty: Boolean = items.isEmpty

  // Extra operations

  // duplicate the top element
  def duplicate: Stack[A] = items match {
    case Nil => this
    case h :: t => Stack(h :: h :: t)
  }

  // swap top two items
  def swap: Stack[A] = items match {
    case Nil => Stack(Nil)
    case x :: Nil => Stack(List(x))
    case x :: y :: zs => Stack(y :: x :: zs)
  }

  // rotate the topmost n elements so the topmost element becomes the nth from the top,
  // the second from the top becomes the top, etc.
  def rotate(n: Int): Stack[A] = {
    val (topmostN, bottom) = items.splitAt(n)
    topmostN match {
      case Nil => this
      case h :: t => Stack(t ::: List(h) ::: bottom)
    }
  }

  // pop n elements from the stack
  def popN(n: Int): (List[A], Stack[A]) =
    (items.take(n), Stack(items.drop(n)))

}
