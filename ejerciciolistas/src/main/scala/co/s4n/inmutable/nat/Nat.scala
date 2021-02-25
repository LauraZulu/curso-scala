package co.s4n.inmutable.nat

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat {

def fromNatToInt(nat:Nat):Int = nat match{
 case Cero => 0
 case Suc(nat) => 1 + fromNatToInt(nat)
}

def fromIntToNat(int:Int):Nat = int match {
 case 0 => Cero
 case n => Suc(fromIntToNat(n-1))
}}
