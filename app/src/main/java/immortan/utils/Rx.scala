package immortan.utils

import scala.concurrent.duration._
import rx.lang.scala.schedulers.IOScheduler
import rx.lang.scala.Observable


object Rx {
  def initDelay[T](next: Observable[T], startMillis: Long, timeoutMillis: Long): Observable[T] = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 10L) 10L else adjustedTimeout
    Observable.just(null).delay(delayLeft.millis).flatMap(_ => next)
  }

  def retry[T](obs: Observable[T], pick: (Throwable, Int) => Duration, times: Range): Observable[T] =
    obs.retryWhen(_.zipWith(Observable from times)(pick) flatMap Observable.timer)

  def repeat[T](obs: Observable[T], pick: (Unit, Int) => Duration, times: Range): Observable[T] =
    obs.repeatWhen(_.zipWith(Observable from times)(pick) flatMap Observable.timer)

  def ioQueue: Observable[Null] = Observable.just(null).subscribeOn(IOScheduler.apply)

  def incSec(errorOrUnit: Any, next: Int): Duration = next.seconds

  def incHour(errorOrUnit: Any, next: Int): Duration = next.hours
}
