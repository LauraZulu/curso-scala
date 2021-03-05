package co.s4n.inmutable.list

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[+A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree extends App {
def size[A](tree:Tree[A]):Int = tree match {
  case Leaf(_) => 1
  case Branch(left,right) => 1 + size(left) + size(right)
}
  /*def depth[A](tree:Tree[A]):Int =  {
    def depthAux[A](tree:Tree[A], acuml:Int, acumr:Int):Int = tree match {
    case Branch(Leaf(_),Leaf(_))=> 1
    case Branch(left,Leaf(_)) => acuml
    case Branch(Leaf(_),right) => acumr
    //case Branch(left,right) => depthAux(left,acuml+1,acumr)  depthAux(right,acumr,acuml)
      }
  }

  println(depth(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))))
  println(depth(Leaf(10)))
  println(depth(Branch(Leaf(10),Leaf(20))))
  println(depth(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))))*/
  println(size((Leaf(10))))
  println(size(Branch(Leaf(10),Leaf(20))))
  println(size(Branch(Branch(Leaf(10),Leaf(20)),Leaf(30))))
}
