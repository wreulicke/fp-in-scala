package com.github.wreulicke

import java.security.NoSuchAlgorithmException

import org.scalatest.FreeSpec

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.Try

class FutureTest extends FreeSpec {

  "test" - {
    "hogehoge" in {
      val x = Future.fromTry {
        Try {
          throw new NoSuchAlgorithmException()
        }
      }

      assert(x.isCompleted == true)
      Await.result(x, Duration.Inf)
    }
  }

}
