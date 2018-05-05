package com.mchange.sc.v3;

package object failable {
  class UnhandledFailureException( failure : Failed[_] ) extends Exception( failure.toString, failure.source match { case th : Throwable => th; case _ => null } );

  val LineSeparator              = scala.util.Properties.lineSeparator;
  val StackTraceElementSeparator = "  " + LineSeparator 
}
