package co.s4n.inmutable.list

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

    def and(lst:List[Boolean]):Boolean = lst match{
    case Nil => true
    case Const(true,t) => and(t)
    case Const(false,_) => false
    }

    def or(lst:List[Boolean]):Boolean = lst match{
    case Nil  => false
    case Const(false,Nil) => false
    case Const(false,t) => or(t)
    case Const(true,t) => true
    }

    def max(lst:List[Int]):Int = {
    	def maxp(lst:List[Int], max:Int):Int = lst match{
	case Nil => max
	case Const(h,t) => maxp(t, if (h > max) h else max)
	}
	maxp(tail(lst), head(lst))
    }
  def min(lst:List[Long]):Long = {
    def minP(lst:List[Long], min:Long):Long = lst match{
      case Nil => min
      case Const(h,t) => minP(t, if(h < min) h else min)
    }
    minP(tail(lst), head(lst))
  }
    def minMax(lst:List[Double]):(Double,Double) = {
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
  }