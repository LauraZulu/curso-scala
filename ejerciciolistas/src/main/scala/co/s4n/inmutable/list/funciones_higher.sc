import co.s4n.inmutable.list.{Const, List, Nil}
def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = as match {
  case Nil => z
  case Const(h,t) => f(h,foldRight(t,z)(f))
}
foldRight(List(9L,6L,7L), Nil:List[Long])(Const(_,_))

object comp {
  def cuadrado(float: Float):Double ={
      cubo(float)
  }
  def cubo(double: Double):Double = {
   val x = math.pow(double,3)
    x
  }
}
comp.cuadrado(50)
object comp2 {
  def cuadrado(long: Long):Double ={
    cubo(long)
  }
  def cubo(long: Long):Double = {
    val x = math.pow(long,3)
    x
  }
}
comp2.cuadrado(5000000)