package com.gu.openplatform.contentapi.parser

import com.gu.openplatform.contentapi.model._
import net.liftweb.json.JsonParser._
import net.liftweb.json.DefaultFormats
import java.util.concurrent.Future
import com.gu.openplatform.contentapi._

trait JsonParser {
  implicit val formats = DefaultFormats + new JodaJsonSerializer

  def parseSearch(jsonFuture: Future[String]): Future[SearchResponse] = wrapFuture(jsonFuture) { json =>
    (parse(json) \ "response").extract[SearchResponse]
  }
  def parseTags(jsonFuture: Future[String]): Future[TagsResponse] = wrapFuture(jsonFuture) { json =>
    (parse(json) \ "response").extract[TagsResponse]
  }
  def parseSections(jsonFuture: Future[String]): Future[SectionsResponse] = wrapFuture(jsonFuture) { json =>
    (parse(json) \ "response").extract[SectionsResponse]
  }
  def parseFolders(jsonFuture: Future[String]): Future[FoldersResponse] = wrapFuture(jsonFuture) { json =>
    (parse(json) \ "response").extract[FoldersResponse]
  }
  def parseItem(jsonFuture: Future[String]): Future[ItemResponse] = wrapFuture(jsonFuture) { json =>
    (parse(json) \ "response").extract[ItemResponse]
  }
}

object JsonParser extends JsonParser