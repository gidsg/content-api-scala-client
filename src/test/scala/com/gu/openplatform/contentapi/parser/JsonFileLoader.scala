package com.gu.openplatform.contentapi.parser

import io.Source
import java.util.concurrent.{TimeUnit, Future}

object JsonFileLoader {
  def loadFile(filename: String) = new Future[String]{
    def cancel(mayInterruptIfRunning: Boolean) = false
    def isCancelled = false
    def isDone = true
    def get() = Option(getClass.getResourceAsStream(filename))
            .map(Source.fromInputStream(_, "UTF-8").mkString)
            .getOrElse(error("could not load file " + filename))
    def get(timeout: Long, unit: TimeUnit) = get
  }
}