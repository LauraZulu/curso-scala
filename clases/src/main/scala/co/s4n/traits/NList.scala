package co.s4n.traits

sealed trait NList[+A]

case class Elem[A](elem: A) extends NList[A]
case class Const[A](lst:List[NList[A]]) extends NList[A]

object NList{
  /*def flatten[A](lst:List[NList[A]]):NList[A] = {
    def flattenAux[A](lst:List[NList[A]], acum:NList[A]):NList[A] = {
    lst match {
      case Elem(_) => acum
      case lst:List[NList[A]] => NList[A]
    }
    flattenAux(lst,)
    }
  }*/
}
