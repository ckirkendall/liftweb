package net.liftweb.util;

object Can {
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }
  
  implicit def can2Iterable[T](in: Can[T]): Iterable[T] = in.toList
  implicit def option2Can[T](in: Option[T]): Can[T] = Can(in)
}

sealed abstract class Can[+A] extends Product {
    def isEmpty: Boolean

    def open_! : A

    def openOr[B >: A](default: => B): B = default

    def map[B](f: A => B): Can[B] = Empty

    def flatMap[B](f: A => Can[B]): Can[B] = Empty

    def filter(p: A => Boolean): Can[A] = this
    
    def foreach(f: A => Any): Unit = {}

    def or[B >: A](alternative: => Can[B]): Can[B] = alternative
    
    def elements: Iterator[A] = Iterator.empty

    def toList: List[A] = Nil
      
    def ?~(msg: String): Can[A] = this
        
    def ?~!(msg: String): Can[A] = ?~(msg)
            
    def isDefined: Boolean = !isEmpty

    def failMsg(msg: String): Can[A] = ?~(msg)

    def compoundFailMsg(msg: String): Can[A] = ?~!(msg)

    def filterMsg(msg: String)(p: A => Boolean): Can[A] = filter(p) ?~ msg
        
    def run[T](in: T)(f: (T, A) => T) = in
    
    def pass(f: Can[A] => Any) = {f(this) ; this}
    
    def $(f: Can[A] => Any) = pass(f)
    
    def choice[B](f1: A => Can[B])(f2: => Can[B]): Can[B] = this match {
    case Full(x) => f1(x)
    case _ => f2
  }
}

final case class Full[+A](value: A) extends Can[A] {
  def isEmpty: Boolean = false 

  def open_! : A = value

  override def openOr[B >: A](default: => B): B = value

  override def map[B](f: A => B): Can[B] = Full(f(value))

  override def flatMap[B](f: A => Can[B]): Can[B] = f(value)

  override def filter(p: A => Boolean): Can[A] = if (p(value)) this else Empty
  
  override def foreach(f: A => Any): Unit = f(value)

  override def or[B >: A](alternative: => Can[B]): Can[B] = this

  override def elements: Iterator[A] = Iterator.fromValues(value)

  override def toList: List[A] = List(value)
  
  override def run[T](in: T)(f: (T, A) => T) = f(in, value)
}

case object Empty extends EmptyCan[Nothing]

abstract class EmptyCan[+A] extends Can[A] {

  def isEmpty: Boolean = true 

  def open_!  = throw new NullPointerException("Trying to open an empty can")

  override def openOr[B >: A](default: => B): B = default

  override def filter(p: A => Boolean): Can[A] = this
  
  override def or[B >: A](alternative: => Can[B]): Can[B] = alternative

  override def ?~(msg: String) = Failure(msg, Empty, Nil)
}

case class Failure(msg: String, exception: Can[Throwable], chain: List[Failure]) extends EmptyCan[Nothing] {
  type A = Nothing
  
  override def ?~(msg: String) = this
      
  override def ?~!(msg: String) = Failure(msg, Empty, this :: chain)
      
  override def map[B](f: A => B): Can[B] = this

  override def flatMap[B](f: A => Can[B]): Can[B] = this
      
  def messageChain: String = (this :: chain).map(_.msg).mkString(" <- ")
}

// vim: set ts=2 sw=2 et: