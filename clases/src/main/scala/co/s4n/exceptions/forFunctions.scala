package co.s4n.exceptions

object forFunctions extends App {

  def fallingFn(i:Int):Int ={
    val y:Int = throw new Exception("fail!")
    try {
      val x = 42 + i
      x + y
    }
    catch {case e: Exception => 43}
  }
  def fallingFn1(i:Int):Int ={
    try {
      val x = 42 + i
      val y:Int = throw new Exception("fail!")
      x + y
    }
    catch {case e: Exception => 43}
  }
 // Implementación de map con for
  def mapa[A,B](list: List[A])(f:(A => B)):List[B] = {
    for {
      x <- list
    } yield (f(x))
  }
  // Implementación de filtro con for
  def filtro[A](list: List[A])(p: (A) => Boolean):List[A] = {
    for {
      x <- list
      if p(x)
    }yield x
  }
// Funcion auxiliar para calcular los divisores de un número
  def divideA(d:Int,n:Int):Boolean = n % d == 0

// Funcion para calcular los divisores de un número

  def divisoresDe(n:Int):List[Int] = for {
    x <- List.range(1,n+1)
    if divideA(x,n)
  }yield (x)

  // Funcion para calcular los numeros primos utilizando la funcion de divisores
  def esPrimo(n:Int):Boolean=divisoresDe(n)==List(1,n)
  esPrimo(7)

  //Funcion para calcular la longitud de una lista usando for
  def miLongitud[A](lst:List[A]):Int =
    (for{
      _ <- lst
    }yield ((a:Int) => a + 1)).foldLeft(0)((e,f) => f(e))

// Funcion para obtener el ultimo elemento de un array
  def myLast1[A](lst:List[A]):A = (for{
    xi <- lst
  } yield((a:A) => xi)).foldLeft(lst.head)((e,f) => f(e))

  // Funcion para obtener el n elemento de la lista
  def elementAt[A](n:Int,lst:List[A]) = (for {
    x <- lst
  }yield ((t:(Int, Option[A]))=> (t._2) match {
    case None => if (t._1 == n) (t._1,Some(x)) else (t._1 + 1,None)
    case Some(x) => (t._1,Some(x))
  })).foldLeft((0,None:Option[A]))((e,f) => f(e))._2.getOrElse(- 1)

// Función que crea una copia de la función
  def copy[A](lst:List[A]):List[A] = (for {
    x <- lst
  }yield ((xs:List[A]) => x :: xs)).foldRight(Nil:List[A])((f,e)=>f(e))

// Funcion para encontrar el reverso de una lista
  def reverse[A](lst:List[A]):List[A] = (for {
    x <- lst
  }yield ((xs:List[A]) => x :: xs)).foldLeft(Nil:List[A])((e,f)=>f(e))
  reverse(List(1,2,3,4,5))

}
