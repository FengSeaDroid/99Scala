package list

object P01 extends App{
	def last[A](xs:List[A]):A = xs match{
	  case x::Nil => x
	  case _::y => last(y)
	}
	
	
}