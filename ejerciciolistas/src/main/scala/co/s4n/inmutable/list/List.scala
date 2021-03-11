package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Const[+A](h: A, t:List[A]) extends List[A]
  
object List {
  // A* seq[A]
  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(h, t) => t
  }

  def head[A](lst: List[A]): A = lst match {
    case Const(h, t) => h
  }

  def headOption[A](lst: List[A]): Option[A] = lst match {
    case Nil => None
    case Const(h, _) => Some(h)
  }

  def sum(lst: List[Int]): Int = lst match {
    case Nil => 0
    case Const(h, t) => h + sum(t)
  }

  @tailrec
  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => true
    case Const(true, t) => and(t)
    case Const(false, _) => false
  }

  @tailrec
  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(false, Nil) => false
    case Const(false, t) => or(t)
    case Const(true, _) => true
  }

  def max(lst: List[Int]): Int = {
    @tailrec
    def maxP(lst: List[Int], max: Int): Int = lst match {
      case Nil => max
      case Const(h, t) => maxP(t, if (h > max) h else max)
    }

    maxP(tail(lst), head(lst))
  }

  def min(lst: List[Long]): Long = {
    @tailrec
    def minP(lst: List[Long], min: Long): Long = lst match {
      case Nil => min
      case Const(h, t) => minP(t, if (h < min) h else min)
    }

    minP(tail(lst), head(lst))
  }

  def minMax(lst: List[Double]): (Double, Double) = {
    @tailrec
    def minMaxP(lst: List[Double], mm: (Double, Double)): (Double, Double) = lst match {
      case Nil => mm
      case Const(h, t) => minMaxP(t, (if (h < mm._1) h else mm._1, if (h > mm._2) h else mm._2))
    }

    minMaxP(tail(lst), (head(lst), head(lst)))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  // ADD element
  def const[A](h: A, t: List[A]): List[A] = Const(h, t)

  // ADD element at the end
  def addEnd[A](h: A, t: List[A]): List[A] = t match {
    case Nil => Const(h, Nil)
    case _ => Const(head(t), addEnd(h, tail(t)))
  }

  def addEndOther[A](t: List[A], elem: A): List[A] = t match {
    case Nil => Const(elem, Nil)
    case Const(h, t) => Const(h, addEndOther(t, elem))
  }

  def addEndTuple[A, B](t: List[(A, B)], elem: A, elem1: B): List[(A, B)] = t match {
    case Nil => Const((elem, elem1), Nil)
    case Const(h, t) => Const(h, addEndTuple(t, elem, elem1))
  }

  //  CONCATENATE LISTS using tuples
  def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (lst1, Nil) => lst1
    case (Nil, lst2) => lst2
    case (Const(h, t), lst2) => Const(h, append(t, lst2))
  }

  //REMOVE n elements from a list
  @tailrec
  def drop[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (0, lst) => lst
    case (n, Nil) => Nil
    case (n, Const(h, t)) => drop(n - 1, t)
  }

  // RETURN a list with n elements
  def take[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def takeP[A](n: Int, lst: List[A], acum: List[A]): List[A] = (n, lst) match {
      case (0, lst) => acum
      case (n, Nil) => Nil
      case (n, Const(h, t)) => takeP(n - 1, t, addEndOther(acum, h))
    }

    takeP(n, lst, Nil)
  }

  // REMOVE last element of the list
  def init[A](lst: List[A]): List[A] = {
    @tailrec
    def initP[A](n: Int, lst: List[A], acum: List[A]): List[A] = (n, lst) match {
      case (_, Nil) => Nil
      case (1, lst) => acum
      case (n, Const(h, t)) => initP(n - 1, t, addEndOther(acum, h))
    }

    initP(length(lst), lst, Nil)
  }

  //SPLIT a list in two
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitTail[A](n: Int, lst: List[A], lstAcum: List[A]): (List[A], List[A]) = (n, lst) match {
      case (0, lst) => (lstAcum, lst)
      case (n, Nil) => (Nil, lstAcum)
      case (n, Const(h, t)) => splitTail(n - 1, t, addEndOther(lstAcum, h))
    }

    splitTail(n, lst, Nil)
  }

  def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = {
    @tailrec
    def zipLength[A, B](lst1: List[A], lst2: List[B], acum: List[(A, B)]): List[(A, B)] = (lst1, lst2) match {
      case (Nil, Nil) => Nil
      case (Nil, lst2) => acum
      case (lst1, Nil) => acum
      case (Const(h, t), Const(h1, t1)) => zipLength(t, t1, addEndTuple(acum, h, h1))
    }

    zipLength(lst1, lst2, Nil)
  }

  def unZip[A, B](lst1: List[(A, B)]): (List[A], List[B]) = {
    @tailrec
    def unZipLength[A, B](lst1: List[(A, B)], acum1: List[A], acum2: List[B]): (List[A], List[B]) = lst1 match {
      case Nil => (acum1, acum2)
      case Const((h, h1), t) => unZipLength(t, addEndOther(acum1, h), addEndOther(acum2, h1))
    }

    unZipLength(lst1, Nil, Nil)
  }

  def reverse {
    @tailrec
    def lastItem[A](lst: List[A], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case Const(h, t) => lastItem(t, Const(h, acum))
    }

    def intersperse[A](elem: A, lst: List[A]): List[A] = {
      @tailrec
      def addIntersperse[A](elem: A, lst: List[A], acum: List[A]): List[A] = lst match {
        case Nil => Nil
        case Const(h, Nil) => addEndOther(acum, h)
        case Const(h, t) => addIntersperse(elem, t, addEndOther(addEndOther(acum, h), elem))
      }

      addIntersperse(elem, lst, Nil)
    }

    def concat[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
      case (Nil, Nil) => Nil
      case (lst1, Nil) => lst1
      case (Nil, lst2) => lst2
      case (Const(h, t), lst2) => Const(h, append(t, lst2))
    }

    // Currying arguments separated
    @tailrec
    def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
      // case Nil => Nil
      case Const(h, t) if f(h) => dropWhile(t)(f)
      case _ => lst
    }

    // HIGHER ORDER FUNCTIONS

    def reduce(lst: List[Int], z: Int)(f: (Int, Int) => Int): Int = lst match {
      case Nil => z
      case Const(h, t) => f(h, reduce(t, z)(f))
    }

    def sumR(lst: List[Int]) = reduce(lst, 0)((x, y) => x + y)

    def mulR(lst: List[Int]) = reduce(lst, 1)((x, y) => x * y)

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Const(h, t) => f(h, foldRight(t, z)(f))
    }

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Const(h, t) => foldLeft(t, f(z, h))(f)
      case Nil => z
    }

    def sumL(lst: List[Int]) = foldLeft(lst, 0)(_ + _)

    def prodL(lst: List[Int]) = foldLeft(lst, 1)(_ * _)

    // For a generic list
    def sumList(lst: List[Int]) = foldRight(lst, 0)((x, y) => x + y)

    def mulList(lst: List[Int]) = foldRight(lst, 1)((x, y) => x * y)

    def sumUno(lst: List[Int]): List[Int] = foldRight(lst, Nil: List[Int])((elem, lst) => Const(elem + 1, lst))

    def mapGen[A, B](lst: List[A])(f: A => B): List[B] = lst match {
      case Nil => Nil
      case Const(h, t) => Const(f(h), mapGen(t)(f))
    }

    def mapR[A, B](lst: List[A])(f: A => B): List[B] = foldRight(lst, Nil: List[B])((x, y) => Const(f(x), y))

    def lengthR[A](lst: List[A]): Int = foldRight(lst, 0)((x, y) => 1 + y)

    def andR(lst: List[Boolean]): Boolean = foldRight(lst, true)((x, y) => if (x && y) true else false)

    @tailrec
    def takeWhile[A](lst: List[A])(p: A => Boolean): List[A] = lst match {
      case Nil => lst
      case Const(h, t) if p(h) => takeWhile(t)(p)
    }

    def takeWhileR[A](lst: List[A])(p: A => Boolean): List[A] = foldRight(lst, Nil: List[A])((x, y) => if (p(x)) Const(x, y) else Nil)

    def dropWhileL[A](lst: List[A])(p: A => Boolean): List[A] = foldLeft(lst, (true: Boolean, Nil: List[A]))((x, y) => x match {
      case (true, _) => if (p(y)) (true, x._2) else (false, addEndOther(x._2, y))
      case (false, _) => if (p(y)) (false, addEndOther(x._2, y)) else (false, addEndOther(x._2, y))
    })._2

    def filterR[A](lst: List[A])(p: A => Boolean): List[A] = foldRight(lst, Nil: List[A])((x, y) => if (p(x)) Const(x, y) else y)

    def unzipR[A, B](lst: List[(A, B)]): (List[A], List[B]) = foldRight(lst, (Nil: List[A], Nil: List[B]))((x, y) => (Const(x._1, y._1), Const(x._2, y._2)))

    // Function zip with f defined
    def unzipOther[A, B](lst: List[(A, B)]): (List[A], List[B]) = {
      def f(x: (A, B), y: (List[A], List[B])): (List[A], List[B]) = {
        (Const(x._1, y._1), Const(x._2, y._2))
      }

      foldRight(lst, (Nil: List[A], Nil: List[B]))(f)
    }

    def lengthL[A](lst: List[A]): Int = foldLeft(lst, 0)((x, y) => 1 + x)

    def andL(lst: List[Boolean]): Boolean = foldLeft(lst, true)((x, y) => if (x && y) true else false)

    def takeWhileL[A](lst: List[A])(p: A => Boolean): List[A] = {
      def f(b: (Boolean, List[A]), a: A): (Boolean, List[A]) = b match {
        case (true, lst) => if (p(a)) (true, addEndOther(lst, a)) else (false, lst)
        case (false, lst) => b
      }

      foldLeft(lst, (true, Nil: List[A]))(f)._2
    }

    def filterL[A](lst: List[A])(p: A => Boolean): List[A] = foldLeft(lst, Nil: List[A])((x, y) => if (p(y)) addEndOther(x, y) else x)

    def unzipL[A, B](lst: List[(A, B)]): (List[A], List[B]) = foldLeft(lst, (Nil: List[A], Nil: List[B]))((x, y) => (addEndOther(x._1, y._1), addEndOther(x._2, y._2)))

  }
}