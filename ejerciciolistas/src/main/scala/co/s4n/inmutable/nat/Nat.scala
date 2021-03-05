package co.s4n.inmutable.nat

import scala.annotation.tailrec

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat

object Nat {

 def fromNatToInt(nat:Nat):Int = nat match{
  case Cero => 0
  case Suc(nat) => 1 + fromNatToInt(nat)
 }

 def fromIntToNat(int:Int):Nat = int match {
  case 0 => Cero
  case n => Suc(fromIntToNat(n-1))
 }
 // with Keiny

 def addNat(nat1: Nat, nat2: Nat): Nat = (nat1, nat2) match {
  case (Cero, n2) => n2
  case (Suc(n1), n2) => Suc(addNat(n1, n2))
 }

 def prodNat(nat1:Nat,nat2:Nat): Nat = {
  @tailrec
  def prodNatAux(nat1:Nat,nat2:Nat,acum:Nat): Nat = (nat1,nat2) match {
   case (Cero,_) => Cero
   case (_,Cero) => Cero
   case (Suc(Cero),n2) => addNat(n2,acum)
   case (Suc(n1), n2) => prodNatAux(n1, n2, addNat(n2, acum))
  }
  prodNatAux(nat1,nat2,Cero)
 }

}

