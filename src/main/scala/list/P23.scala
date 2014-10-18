package list

object P23 extends App {
  import P20.removeAt
  def randomSelect[A](n: Int, ls: List[A]): List[A] = n match {
    case n if (n < 0) => throw new IllegalArgumentException
    case 0 => Nil
    case n => {
      val random = new util.Random
      val result = removeAt(random.nextInt(ls.length), ls)
      result._2 :: randomSelect(n - 1, result._1)
    }
  }
  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
}

object P24 extends App {
  import P23.randomSelect
  import P22.range
  def lotto(n: Int, max: Int) = randomSelect(n, range(1, max))
  println(lotto(6, 49))
}

object P25 extends App {
  import P23._
  //quadratic
  def randomPermute[A](ls: List[A]) = randomSelect(ls.length, ls)
  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
}
object P26 extends App {
  //permutations
  def permutations[A](n: Int, ls: List[A]): List[List[A]] = n match {
    case 1 => ls.map(x => List(x))
    case n => ls.foldLeft(List[List[A]]())((z, x) => z ::: (permutations(n - 1, ls.filterNot(y => y == x)).map(elem => x :: elem)))
  }
  println(permutations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length)

  //combinations alternative solution
  def combinations[A](n: Int, ls: List[A]): List[List[A]] = (n, ls) match {
    case (1, _) => ls.map(x => List(x))
    case (_, Nil) => Nil
    case (n, x :: xs) => combinations(n - 1, xs).map(elem => x :: elem) ::: combinations(n, xs)
  }
  println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length)
}

object P27 extends App {
  import P26._
  def group[A](number: List[Int], ls: List[A]): List[List[List[A]]] = number match {
    case Nil => List(Nil)
    case n :: ns =>
      combinations(n, ls) flatMap (x => group(ns, ls diff x) map(x :: _))
  }
  println(group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
}

object P28 extends App {
  def lsort[A](ls:List[List[A]]):List[List[A]]=
    ls.sortBy(x=>x.length)
  println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))

  import P10.encode
  def lsortFreq[A](ls:List[List[A]]):List[List[A]]={
    val encoded = encode(ls map(x=>x.length) sorted)
    ls sortBy(x=>encoded.find(_._2==x.length).get._1)
  }
  println(lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
}