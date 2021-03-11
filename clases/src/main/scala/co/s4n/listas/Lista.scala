package co.s4n.listas

import scala.annotation.tailrec

object lista extends App {

  //Función para encontrar las posibles combinaciones de una lista
  def subs[A](lst:List[A]):List[List[A]]= {
    @tailrec
    def subsAux(n:Int, lst:List[A], acum:List[List[A]]):List[List[A]] = n match {
      case -1 => acum
      case n => subsAux(n-1,lst, lst.combinations(n).toList :::acum)
    }
    subsAux(lst.length,lst,List())
  }

  //Otra implementación de función para encontrar las posibles combinaciones de una lista
  def subs2[A](lst:List[A]):List[List[A]]= {
    val a = lst match {
      case Nil => List(Nil)
      case head :: tail => subs2(tail).map(head :: _) ::: subs2(tail)
    }
    a.sortBy(_.length)
  }

  //Función de las posibles permutaciones de una lista
  def permutaciones[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => Nil
    case List(x) => List(List(x))
    case _ => lst.flatMap(x => permutaciones(lst.filterNot(_==x)).map(p => x :: p))
  }
  println(permutaciones(List(1,2)))
  //Función de las posibles permutaciones de una lista utilizando la funcion barajar
  def barajar[A](a:A,lst: List[A]): List[List[A]] = lst match {
    case Nil => List(List(a))
    case x :: xs => (a :: (x :: xs)):: barajar(a,xs).map(x::_)
  }
  def permutaciones2[A](lst: List[A]): List[List[A]] = lst match {
    case Nil => List(Nil)
    case x :: xs => permutaciones2(xs).flatMap(barajar(x,_))
  }

  // Funciones para encontrar el ultimo elemento de una lista
  @tailrec
  def lastF[A](lst:List[A]):A = lst match {
    case List(a) => a
    case lst => lastF(lst.splitAt(lst.length - 1)._2)
  }
  def lastF2[A](lst:List[A]):A = {
    def lastF2Aux[A](n:Int,lst:List[A]):A = (n,lst) match {
      case (2,x::xs) => x
      case (n,x::xs) => lastF2Aux(n-1,xs)
    }
    lastF2Aux(lst.length,lst)
  }
  def myLast2[A](lst:List[A]):List[A] = lst match {
    case x:: y:: Nil => List(x,y)
    case _ :: y => myLast2(y)
  }

  // Funcion para retornar el n elemento de la lista
  def returnElemen[A](n:Int,lst:List[A]):A = (n,lst) match {
    case (1, x :: _) => x
    case (n,x::xs) => returnElemen(n-1,xs)
  }

  //Función para encontrar la longitud de una lista
  def lengthL[A](lst: List[A]): Int = lst.foldLeft(0)((x,_)=> 1 + x)
  def lengthR[A](lst: List[A]): Int = lst.foldRight(0)((_,y)=> 1 + y)

  // Función para encontrar el reverso de una lista
  def reversel[A](lst: List[A]): List[A] = {
    def reverseAux[A](lst:List[A],acum:List[A]):List[A] = lst match {
      case Nil => acum
      case x :: xs => reverseAux(xs,x :: acum)
    }
    reverseAux(lst, List())
  }
 // Define si una función es palindrome
  def isPalindrome[A](lst:List[A]):Boolean = lst == reversel(lst)

  //Función para agrupar elementos iguales
  def pack[A](lst:List[A]):List[List[A]] = {
    def packAux[A](lst:List[A],acumL:List[A],acum:List[List[A]]):List[List[A]] = lst match {
      case Nil => acum
      case x :: Nil => packAux(Nil,Nil,acum ::: List(x::acumL))
      case x::xs => if(x == xs.head) packAux(xs,x::acumL,acum) else packAux(xs,Nil, acum ::: List(x::acumL))
    }
    packAux(lst,Nil,List())
  }

  def encode[A](lst:List[A]):List[(Int,A)]={
    def encodeAux[A](lista:List[List[A]],acum:List[(Int,A)]):List[(Int,A)] = lista match {
      case Nil => acum
      case x::xs => encodeAux(xs, acum:::List((x.length,x.head)))
    }
    encodeAux(pack(lst),Nil)
  }
  
// Función para duplicar elementos de una lista
  def dupli[A](lst:List[A]):List[A] = {
    def dupliAux[A](lst:List[A],acum:List[A]):List[A] = lst match {
      case Nil => acum.reverse
      case x::xs => dupliAux(xs,x::x::acum)
    }
    dupliAux(lst,Nil)
  }

  sealed trait Valores[+A]
  case class Multiple[A](veces:Int,unA:A) extends Valores[A]
  case class Unico[A](unA:A) extends Valores[A]

  def encodeModified[A](valores: Valores[A]):Valores[A] = valores match {
    case Unico(a) => Unico(a)
    case Multiple(veces,unA) => ???

}}
