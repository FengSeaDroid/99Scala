package list

object P01 extends App {
  def last[A](xs: List[A]): A = xs match {
    case x :: Nil => x
    case _ :: y => last(y)
    case _ => throw new IllegalArgumentException
  }

  println(last(List(1, 1, 2, 3, 5, 8)))
}

object P02 extends App {
  def penultimate[A](xs: List[A]): A = xs match {
    case x :: y :: Nil => x
    case _ :: y => penultimate(y)
    case _ => throw new IllegalArgumentException
  }

  println(penultimate(List(1, 1, 2, 3, 5, 8)))
}

object P03 extends App {
  def nth[A](n: Int, xs: List[A]): A = (n, xs) match {
    case (0, xs) => xs.head
    case (_, Nil) => throw new IllegalArgumentException
    case (n, xs) => nth(n - 1, xs.tail)
    case _ => throw new IllegalArgumentException
  }
  println(nth(2, List(1, 1, 2, 3, 5, 8)))
}

object P04 extends App {
  def length[A](xs: List[A]) = {
    def lengthHelper[A](n: Int, xs: List[A]): Int = (n, xs) match {
      case (n, Nil) => n
      case (n, xs) => lengthHelper(n + 1, xs.tail)
    }
    lengthHelper(0, xs)
  }
  println(length(List(1, 1, 2, 3, 5, 8)))
}

object P05 extends App {
  def reverse[A](xs: List[A]) = {
    def reverseAcc(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case x :: ls => reverseAcc(ls, x :: acc)
    }
    reverseAcc(xs, List[A]())
  }
  println(reverse(List(1, 1, 2, 3, 5, 8)))
}

object P06 extends App {
  def isPalindrome[A](ls: List[A]) = ls == ls.reverse
  println(isPalindrome(List(1, 2, 3, 2, 1)))
}

object P07 extends App {
  def flatten(ls:List[Any]):List[Any]= ls match {
    case Nil => List[Any]()
    case (x:List[Any])::xs => flatten(x):::flatten(xs)
    case (x:Any)::xs => x::flatten(ls.tail)
  }
  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
}