package com.gu.openplatform

import java.util.concurrent.{TimeUnit, Future}


package object contentapi {
  def wrapFuture[A,B](delegate: Future[A])(f: A => B) = new Future[B]{
    def cancel(mayInterruptIfRunning: Boolean) = delegate.cancel(mayInterruptIfRunning)
    def isCancelled = delegate.isCancelled
    def isDone = delegate.isDone
    def get() = f(delegate.get)
    def get(timeout: Long, unit: TimeUnit) = f(delegate.get(timeout, unit))
  }
}
