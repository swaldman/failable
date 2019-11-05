package com.mchange.sc.v3.failable

import scala.collection._
import scala.util.Try
import scala.util.control.NonFatal

object Failable {
  
  val Empty : Failable[Nothing] = Failed("EmptyFailable")("An attempt to filter or pattern-match a Failable failed, leaving EmptyFailable.", None);

  def sequence[T]( failables : Seq[Failable[T]] ) : Failable[immutable.Seq[T]] = {
    failables.foldLeft( succeed( immutable.Seq.empty[T] ) ){ ( fseq, fnext ) =>
      fseq.flatMap( seq => fnext.map( next => seq :+ next ) )
    }
  }
  def apply[T]( block : =>T ) = Try( block ).toFailable

  def succeed[T]( result : T ) : Failable[T] = Succeeded( result )

  def fail[S : Failed.Source]( source : S, includeStackTrace : Boolean = true ) : Failable[Nothing] = {
    val ms = implicitly[Failed.Source[S]]
    ms.getFailed( source, includeStackTrace )
  }

  /**
    * A utility to re-establish the irrelevant right type as universally acceptable Nothing
    */  
  def refail( prefail : Failed[_] ) : Failable[Nothing] = prefail.asInstanceOf[Failable[Nothing]]

  def flatCreate[T]( op : => Failable[T] ) : Failable[T] = UnitSuccess.flatMap( _ => op )

  // the explicit provision of the implicit Failed.Source.Throwable param is apparently required when the definition of that parameter comes
  // later in the compilation unit... Grrr.
  //
  // See e.g. https://stackoverflow.com/questions/2731185/why-does-this-explicit-call-of-a-scala-method-allow-it-to-be-implicitly-resolved
  val ThrowableToFailed : PartialFunction[Throwable, Failable[Nothing]] = { case scala.util.control.NonFatal( t : Throwable ) => fail( t )( Failed.Source.ForThrowable ) }

  private val UnitSuccess : Failable[Unit] = succeed( () )
}
sealed trait Failable[+T] {
  def isEmpty         : Boolean      = this == Failable.Empty
  def assertResult    : T            = assertSucceeded.result
  def assertSucceeded : Succeeded[T] = {
    try {
      this.asInstanceOf[Succeeded[T]]
    }
    catch {
      case cce : ClassCastException => this.asInstanceOf[Failed[T]].vomit
    }
  }
  def assertFailed    : Failed[T]    = this.asInstanceOf[Failed[T]]
  def assertThrowable : Throwable    = this.assertFailed.toThrowable
  def isFailed        : Boolean      = !this.isSucceeded;
  def asFailed        : Failed[T]    = this.assertFailed
  def asSucceeded     : Succeeded[T] = this.assertSucceeded
  def get             : T            = this.assertResult
  def assert          : T            = this.assertResult

  // monad ops
  def flatMap[U]( f : T => Failable[U] ) : Failable[U]
  def map[U]( f : T => U )               : Failable[U]
  def withFilter( p : T => Boolean )     : Failable[T]

  // extra ops
  def exists( f : T => Boolean )                               : Boolean
  def forall( f : T => Boolean )                               : Boolean 
  def foreach[U]( f : T => U )                                 : Any
  def getOrElse[ TT >: T ]( or : =>TT )                        : TT
  def toOption                                                 : Option[T]
  def toSeq                                                    : immutable.Seq[T]
  def flatten[U](implicit evidence : T <:< Failable[U])        : Failable[U]
  def recover[TT >: T]( f : Failed[T] => TT )                  : Failable[TT]
  def recoverWith[TT >: T]( f : Failed[T] => Failable[TT] )    : Failable[TT]
  def orElse[TT >: T]( other : =>Failable[TT] )                : Failable[TT]
  def fold[X]( ff : Failed[T] => X )( fr : T => X )            : X
  def isSucceeded                                              : Boolean

  def orElseTrace[TT >: T]( other : =>Failable[TT] )           : Failable[TT]
}


object SequenceOfFaileds {
  def apply( previousFailed : Failed[_], currentFailed : Failed[_] ) : SequenceOfFaileds = {
    previousFailed match {
      case Failed( sof : SequenceOfFaileds ) => sof.copy( faileds = (sof.faileds :+ currentFailed) )
      case firstFailed                       => SequenceOfFaileds( immutable.Seq( firstFailed, currentFailed ) )
    }
  }
}
case class SequenceOfFaileds( faileds : immutable.Seq[Failed[_]] )

// kind of yuk, but we've renamed this from "Failure" to "Fail" to avoid inconvenient
// need to qualify names when working with scala.util.Failure.
final object Failed {
  final object Source {
    implicit val ForString = new Failed.Source[String] {
      def getMessage( source : String ) : String = source;
    }
    implicit def forAnyThrowable[T <: Throwable] = new Failed.Source[T] {
      def getMessage( source : T ) : String = s"${source.getClass.getName}: ${source.getMessage()}";

      override def getStackTrace( source : T ) = source.getStackTrace;
    }
    implicit val ForThrowable = this.forAnyThrowable[Throwable]

    implicit val ForSequenceOfFaileds = new Failed.Source[SequenceOfFaileds]{
      def getMessage( source : SequenceOfFaileds ) : String = {
        val messages = source.faileds.map( _.message ).mkString("; ")
        s"Multiple failures: ${messages}"
      }
      override def getStackTrace( source : SequenceOfFaileds ) = {
        source.faileds.last.mbStackTrace match {
          case Some( mbst ) => mbst
          case None         => super.getStackTrace( source )
        }
      }
    }


  }
  trait Source[T] {
    def getMessage( source : T ) : String
    def getStackTrace( source : T ) : Array[StackTraceElement] = Thread.currentThread().getStackTrace()

    def getFailed( source : T, includeStackTrace : Boolean = true ) : Failed[Nothing] = {
      val mbStackTrace = if ( includeStackTrace ) Some( getStackTrace( source ) ) else None
      Failed[Nothing]( source )( getMessage( source ), mbStackTrace )
    }
  }
  def simple( message : String ) : Failed[Nothing] = Failed( message )( message, None );

  val FromThrowable = Failable.ThrowableToFailed
}

import Failable.{ refail, ThrowableToFailed }

final case class Failed[+T]( val source : Any )( val message : String, val mbStackTrace : Option[Array[StackTraceElement]] ) extends Failable[T] {
  override def toString() : String = "Failed: " + mbStackTrace.fold( message ) { stackTrace =>
    (List( message ) ++ stackTrace).mkString( StackTraceElementSeparator )
  }
  def toThrowable : Throwable = {
    source match {
      case t : Throwable => t
      case _             => new NonthrowableFailureException( this );
    }
  }
  def vomit : Nothing = throw this.toThrowable

  // monad ops
  def flatMap[U]( f : T => Failable[U] ) : Failable[U] = refail( this )
  def map[U]( f : T => U )               : Failable[U] = refail( this )
  def withFilter( p : T => Boolean )     : Failable[T] = this

  // extra ops
  def exists( f : T => Boolean )                               : Boolean          = false
  def forall( f : T => Boolean )                               : Boolean          = true
  def foreach[U]( f : T => U )                                 : Any              = ()
  def getOrElse[ TT >: T ]( or : =>TT )                        : TT               = or
  def toOption                                                 : Option[T]        = None
  def toSeq                                                    : immutable.Seq[T] = immutable.Seq.empty[T]
  def flatten[U](implicit evidence : T <:< Failable[U])        : Failable[U]      = refail( this )
  def recover[TT >: T]( f : Failed[T] => TT )                  : Failable[TT]     = try { Succeeded( f( this ) ) } catch ThrowableToFailed
  def recoverWith[TT >: T]( f : Failed[T] => Failable[TT] )    : Failable[TT]     = try { f( this )              } catch ThrowableToFailed
  def orElse[TT >: T]( other : =>Failable[TT] )                : Failable[TT]     = other
  def fold[X]( ff : Failed[T] => X )( fr : T => X )            : X                = ff( this )
  def isSucceeded                                              : Boolean          = false

  def orElseTrace[TT >: T]( other : =>Failable[TT] ) : Failable[TT] = {
    val result = other
    if (result.isFailed) Failable.fail( SequenceOfFaileds( this, result.assertFailed ), false ) else result
  }
}
final case class Succeeded[+T]( result : T ) extends Failable[T] {
  // monad ops
  def flatMap[U]( f : T => Failable[U] ) : Failable[U] = try { f(result)                                 } catch ThrowableToFailed
  def map[U]( f : T => U )               : Failable[U] = try { Succeeded( f(result) )                    } catch ThrowableToFailed
  def withFilter( p : T => Boolean )     : Failable[T] = try { if ( p(result) ) this else Failable.Empty } catch ThrowableToFailed

  // extra ops
  def exists( f : T => Boolean )                               : Boolean           = f(result)
  def forall( f : T => Boolean )                               : Boolean           = f(result)
  def foreach[U]( f : T => U )                                 : Any               = f(result)
  def getOrElse[ TT >: T ]( or : =>TT )                        : TT                = result
  def toOption                                                 : Option[T]         = Some( result )
  def toSeq                                                    : immutable.Seq[T]  = immutable.Seq( result ) 
  def flatten[U](implicit evidence : T <:< Failable[U])        : Failable[U]       = evidence( result )
  def recover[TT >: T]( f : Failed[T] => TT )                  : Failable[TT]      = this
  def recoverWith[TT >: T]( f : Failed[T] => Failable[TT] )    : Failable[TT]      = this
  def orElse[TT >: T]( other : =>Failable[TT] )                : Failable[TT]      = this
  def fold[X]( ff : Failed[T] => X )( fr : T => X )            : X                 = fr( this.result )
  def isSucceeded                                              : Boolean           = true

  def orElseTrace[TT >: T]( other : =>Failable[TT] ) : Failable[TT] = this
}

