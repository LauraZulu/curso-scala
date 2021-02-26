package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Const[+A](h: A, t:List[A]) extends List[A]
  
object List {
    // A* seq[A]
    def length[A](lst:List[A]):Int = lst match {
      case Nil => 0
      case Const(h,t) => 1 + length(t)
    }
    
    def tail[A](lst:List[A]):List[A] = lst match{
      case Nil => Nil
      case Const(h,t) => t
    }
    
    def	head[A](lst:List[A]):A = lst match{
      case Const(h,t) => h
    }
    
    def headOption[A](lst:List[A]):Option[A] = lst match{
      case Nil => None
      case Const(h,_) =>Some(h)
    }

     def sum(lst:List[Int]):Int = lst match{
      case Nil => 0
      case Const(h,t) => h + sum(t)
    }

    @tailrec
    def and(lst:List[Boolean]):Boolean = lst match{
      case Nil => true
      case Const(true,t) => and(t)
      case Const(false,_) => false
    }

    @tailrec
    def or(lst:List[Boolean]):Boolean = lst match{
      case Nil  => false
      case Const(false,Nil) => false
      case Const(false,t) => or(t)
      case Const(true,_) => true
    }

    def max(lst:List[Int]):Int = {
    	@tailrec
      def maxP(lst:List[Int], max:Int):Int = lst match{
      case Nil => max
      case Const(h,t) => maxP(t, if (h > max) h else max)
	    }
    maxP(tail(lst), head(lst))
    }

    def min(lst:List[Long]):Long = {
      @tailrec
      def minP(lst:List[Long], min:Long):Long = lst match{
        case Nil => min
        case Const(h,t) => minP(t, if(h < min) h else min)
      }
    minP(tail(lst), head(lst))
    }

    def minMax(lst:List[Double]):(Double,Double) = {
    	@tailrec
      def minMaxP(lst:List[Double], mm:(Double,Double)):(Double,Double) = lst match {
	    case Nil => mm
	    case Const(h,t) => minMaxP(t, (if (h < mm._1) h else mm._1, if (h > mm._2) h else mm._2))
	  }
    minMaxP(tail(lst), (head(lst), head(lst)))
    }
    
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Const(as.head, apply(as.tail: _*))
    }

  // ADD element
    def const[A](h:A, t:List[A]):List[A] = Const(h,t)
  // ADD element at the end
    def addEnd[A](h:A, t:List[A]):List[A] = t match {
      case Nil => Const(h,Nil)
      case _ => Const(head(t), addEnd(h,tail(t)))
    }

    def addEndOther[A](t:List[A], elem:A):List[A] = t match {
      case Nil => Const(elem,Nil)
      case Const(h,t) => Const(h, addEndOther(t,elem))
    }

  //  CONCATENATE LISTS using tuples
    def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1,lst2) match {
      case (Nil,Nil) => Nil
      case (lst1,Nil) => lst1
      case (Nil, lst2) => lst2
      case (Const(h,t), lst2) => Const(h, append(t,lst2))
    }

  // RETURN a list with n elements
    def init[A](n:Int, lst:List[A]):List[A] =  {
        def initP[A](n:Int, lst:List[A],acum:List[A]):List[A] = (n,lst) match {
        case (0,lst) => acum
        case (n,Nil) => Nil
        case (n, Const(h,t)) => initP(n-1,t,addEndOther(acum,h))
      }
      initP(n,lst,Nil)
    }

  //REMOVE n elements from a list
    def drop[A](n:Int, lst:List[A]):List[A] = (n,lst) match {
      case (0,lst) => lst
      case (n,Nil) => Nil
      case (n,Const(h,t)) => drop(n-1,t)
    }

  //SPLIT a list in two
    def split[A](n:Int, lst:List[A]):(List[A],List[A]) =  {
      def splitTail[A](n:Int,lst:List[A],lstAcum:List[A]):(List[A],List[A])= (n,lst) match{
          case (0,lst) => (lstAcum,lst)
          case (n,Nil) => (Nil,lstAcum)
          case (n,Const(h,t)) => splitTail(n-1,t,addEndOther(lstAcum, h))
      }
      splitTail(n,lst,Nil)
    }
}