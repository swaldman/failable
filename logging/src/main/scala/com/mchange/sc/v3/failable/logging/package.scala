package com.mchange.sc.v3.failable

import com.mchange.sc.v1.log.{MLogger,MLevel};
import com.mchange.sc.v1.log.MLevel._;

package object logging {
  implicit class FailableLoggingOps[T]( val failable : Failable[T] ) extends AnyVal {
    def xlog( level : MLevel, premessage : => String  = "" )( implicit logger : MLogger ) : Failable[T] = {
      def doLog( oops : Failed[T] ) = {
        val pm = premessage; // avoid multiple executions of the by name expression
        val prefix = if ( pm == "" || pm == null ) "" else pm + LineSeparator;
        level.log( prefix + oops )( logger )
      }
      failable match {
        case oops : Failed[T] => doLog( oops );
        case _                => /* ignore */;
      }
      failable
    }
    def logRecover[TT >: T]( level : MLevel, recoveryFunction : Failed[T] => TT, premessage : => String )( implicit logger : MLogger ) : Failable[TT] = {
      xlog( level, premessage )( logger ).recover( recoveryFunction );
    }
    def logRecover[TT >: T]( level : MLevel, recoveryFunction : Failed[T] => TT )( implicit logger : MLogger ) : Failable[TT] = logRecover[TT]( level, recoveryFunction, "" )( logger );

    def logRecoverWithValue[TT >: T]( level : MLevel, recoveryValue : TT, premessage : => String )( implicit logger : MLogger ) : Failable[TT] = {
      xlog( level, premessage )( logger ).recover( _ => recoveryValue );
    }
    def logRecoverWithValue[TT >: T]( level : MLevel, recoveryValue : TT )( implicit logger : MLogger ) : Failable[TT] = logRecoverWithValue( level, recoveryValue, "" )( logger );

    // is the API below just a little too cute?
    def xwarning( premessage : => String = "" )( implicit logger : MLogger ) : Failable[T] = xlog( WARNING, premessage )( logger )
    def xsevere( premessage : => String = "" )( implicit logger : MLogger )  : Failable[T] = xlog( SEVERE, premessage )( logger )
    def xinfo( premessage : => String = "" )( implicit logger : MLogger )    : Failable[T] = xlog( INFO, premessage )( logger )
    def xdebug( premessage : => String = "" )( implicit logger : MLogger )   : Failable[T] = xlog( DEBUG, premessage )( logger )
    def xtrace( premessage : => String = "" )( implicit logger : MLogger )   : Failable[T] = xlog( TRACE, premessage )( logger )

    def xwarn( premessage : => String = "" )( implicit logger : MLogger ) : Failable[T] = xwarning( premessage )( logger )
  }
}
