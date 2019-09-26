package com.github.wreulicke.validation

import monix.eval.{Task, TaskCircuitBreaker}


class MonixTest {

  import scala.util.Random
  import scala.concurrent.duration._

  val circuitBreaker = TaskCircuitBreaker(
    maxFailures = 5,
    resetTimeout = 10 seconds
  )

  def execute(): Task[Int] = {
    exponetialBackoff {
      circuitBreaker.protect {
        Task(Random.nextInt).flatMap {
          case statusCode if statusCode == 503 =>
            Task.raiseError(new IllegalStateException())
          case other =>
            Task.now(other)
        }
      }
    } (3, 1 seconds) {
      case IllegalStateException => true
    }
  }

  def exponetialBackoff[A](source: Task[A])(maxRetries: Int, firstDelay: FiniteDuration)(retryIf: PartialFunction[Throwable, Boolean]): Task[A] = {
    source.onErrorHandleWith {
      ex =>
        if (maxRetries > 0 && retryIf.applyOrElse(ex, _ => false))
          exponetialBackoff(source)(maxRetries-1, firstDelay*2)(retryIf)
            .delayExecution(firstDelay)
        else
          Task.raiseError(ex)
    }
  }

}
