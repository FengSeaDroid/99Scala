package list

object P08 extends App {
  def compress[A](ls: List[A]): List[A] = ls match {
    case x :: Nil => List(x)
    case x :: y :: xs => if (x == y) compress(x :: xs) else x :: compress(y :: xs)
  }
  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P09 extends App {

  def pack[A](ls: List[A]) = {
    def packHelper(n: Int, ls: List[A]): List[List[A]] = ls match {
      case x :: y :: Nil =>
        if (x == y) List(List.fill(n + 1)(x)) else List(List.fill(n)(x), List(y))
      case x :: y :: xs =>
        if (x == y) packHelper(n + 1, y :: xs) else List.fill(n)(x) :: packHelper(1, y :: xs)
    }
    packHelper(1, ls)
  }
  println(pack(List(1, 2, 2, 2, 3, 3, 4, 5, 5)))
  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P10 extends App {
  def encode[A](ls: List[A]): List[(Int, A)] = {
    P09.pack(ls).map(xs => (xs.length, xs.head))
  }
  println(encode(List(1, 2, 2, 2, 3, 3, 4)))
  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P11 extends App {
  def encodeModified[A](ls: List[A]): List[Either[A, (Int, A)]] = {
    P09.pack(ls) map (xs => xs match {
      case x :: Nil => Left(x)
      case xs => Right((xs.length, xs.head))
    })
  }
  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P12 extends App {
  def decode[A](ls: List[(Int, A)]): List[A] = ls match {
    case Nil => List()
    case x :: xs => List.fill(x._1)(x._2) ::: decode(xs)
  }
  println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
}

object P13 extends App {
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    def encodeHelper(n: Int, ls: List[A]): List[(Int, A)] = ls match {
      case x :: y :: Nil => if (x == y) List((n + 1, x)) else List((n, x), (1, y))
      case x :: y :: xs => if (x == y) encodeHelper(n + 1, y :: xs) else (n, x) :: encodeHelper(1, y :: xs)
    }
    encodeHelper(1, ls)
  }
  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P14 extends App {
  def duplicate[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }
  println(duplicate(List('a, 'b, 'c, 'c, 'd)))
}

object P15 extends App {
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case x :: xs => List.fill(n)(x) ::: duplicateN(n, xs)
  }
  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
}