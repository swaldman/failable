package com.mchange.sc.v3

import scala.util.{Failure,Success,Try}

package object failable {
  class UnhandledFailureException( failure : Failed[_] ) extends Exception( failure.toString, failure.source match { case th : Throwable => th; case _ => null } );

  val LineSeparator              = scala.util.Properties.lineSeparator;
  val StackTraceElementSeparator = "  " + LineSeparator

  // the explicit provision of the implicit Failed.Source.ForThrowable param is apparently required when the definition of that parameter comes
  // later in the compilation unit... Grrr.
  //
  // See e.g. https://stackoverflow.com/questions/2731185/why-does-this-explicit-call-of-a-scala-method-allow-it-to-be-implicitly-resolved
  implicit class TryOps[T]( val attempt : Try[T] ) extends AnyVal {
    def toFailable : Failable[T] = attempt match {
      case Success( value )     => Failable.succeed( value );
      case Failure( exception ) => Failable.fail( exception, true )( Failed.Source.ForThrowable );
    }
  }

  implicit class OptionOps[T]( val maybe : Option[T] ) extends AnyVal {
    def toFailable[ U : Failed.Source ]( source : U = "No information available." ) : Failable[T] = {
      maybe match {
        case Some( value )  => Failable.succeed( value );
        case None           => Failable.fail( source, true );
      }
    }
  }

  private val WrappedTrue = Failable.succeed( true )

  implicit class BooleanOps( val b : Boolean ) extends AnyVal {
    def toFailable[ U : Failed.Source ]( source : U = "No information available." ) : Failable[Boolean] = {
      if (b) WrappedTrue else Failable.fail( source )
    }
  }
}
