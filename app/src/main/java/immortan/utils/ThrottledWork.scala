package immortan.utils

import immortan.crypto.Tools.Any2Some
import rx.lang.scala.Subscription

abstract class ThrottledWork[T, V] {
  private var lastWork: Option[T] = None
  private var subscription: Option[Subscription] = None
  def finishedOrNeverStarted: Boolean = subscription.isEmpty
  def work(input: T): rx.lang.scala.Observable[V]
  def process(data: T, res: V): Unit
  def error(error: Throwable): Unit

  private def runOnNext(data: T, res: V): Unit = {
    // First nullify sunscription, then process callback
    subscription = None
    process(data, res)
  }

  def addWork(data: T): Unit = if (subscription.isEmpty) {
    // Previous work has already been finished by now or has never started at all
    // schedule a new one and then look if more work is added once this one is done

    subscription = work(data)
      .doOnSubscribe { lastWork = None }
      .doAfterTerminate(lastWork foreach addWork)
      .subscribe(res => runOnNext(data, res), error)
      .toSome

  } else {
    // Current work has not finished yet
    // schedule new work once this is done
    lastWork = Some(data)
  }

  def replaceWork(data: T): Unit = if (subscription.isEmpty) {
    // Previous work has already finished or was interrupted or has never been started
    subscription = work(data).subscribe(res => process(data, res), error).toSome
  } else {
    // Current work has not finished yet
    // disconnect subscription and replace
    unsubscribeCurrentWork
    replaceWork(data)
  }

  def unsubscribeCurrentWork: Unit = {
    subscription.foreach(_.unsubscribe)
    subscription = None
  }
}