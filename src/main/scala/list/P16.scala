package list

object P16 extends App {
  def drop[A](n: Int, ls: List[A]): List[A] = {
    def dropHelper(n: Int, i: Int, ls: List[A]): List[A] = (i, ls) match {
      case (_, Nil) => Nil
      case (1, x :: xs) => dropHelper(n, n, xs)
      case (i, x :: xs) => x :: dropHelper(n, i - 1, xs)
    }
    dropHelper(n, n, ls)
  }
  println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P17 extends App {

  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil) => throw new IllegalArgumentException
    case (1, x :: xs) => (List(x), xs)
    case (n, x :: xs) => {
      val result = split(n - 1, xs)
      (x :: result._1, result._2)
    }
  }
  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P18 extends App {
  def slice[A](start: Int, end: Int, ls: List[A]): List[A] = (start, end, ls) match {
    case (start, end, _) if (start >= end || start < 0) => throw new IllegalArgumentException
    case (_, end, Nil) if (end >= 0) => throw new IllegalArgumentException
    case (start, end, x :: xs) => {
      if (start == 0 && end == 1) List(x)
      else if (start == 0) x :: slice(0, end - 1, xs)
      else slice(start - 1, end - 1, xs)
    }
  }
  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P19 extends App {
  def rotate[A](pos: Int, ls: List[A]): List[A] = {
    val boundedPos = if (ls.length == 0) 0 else (pos % ls.length + ls.length) % ls.length
    println(boundedPos)
    P18.slice(boundedPos, ls.length, ls) ::: P18.slice(0, boundedPos, ls)
  }
  println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P20 extends App {
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = n match {
    case 0 => (ls.tail, ls.head)
    case n if (n > 0) => {
      val result = removeAt(n - 1, ls.tail)
      (ls.head :: result._1, result._2)
    }
    case _ => throw new IllegalArgumentException
  }
  println(removeAt(3, List('a, 'b, 'c, 'd)))
}

object P21 extends App {
  def insertAt[A](elem: A, n: Int, ls: List[A]): List[A] = n match {
    case 0 => elem :: ls
    case n if (n>0) => ls.head::insertAt(elem,n-1,ls.tail)
    case _ => throw new IllegalArgumentException
  }
  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
}

object P22 extends App {
  def range(start:Int,end:Int):List[Int]= (start,end) match {
    case (x,y) if (x==y) => List(y)
    case (x,y) if (x<y) => x::range(x+1,y)
  }
  println(range(4, 9))
}
