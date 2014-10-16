package list

object P08 extends App {
  def compress[A](ls:List[A]):List[A]= ls match{
    case x::Nil => List(x) 
    case x::y::xs => if(x==y) compress(x::xs) else x::compress(y::xs)
  }
  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P09 extends App{
  
  def pack[A](ls:List[A])={
    def packHelper(n:Int,ls:List[A]):List[List[A]]= ls match {
      case x::y::Nil => 
        if(x==y) List(List.fill(n+1)(x)) else List(List(x),List(y))
      case x::y::xs => 
        if(x==y) packHelper(n+1,y::xs) else List.fill(n)(x)::packHelper(1,y::xs)
    }
    packHelper(1,ls)
  }
  
  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}