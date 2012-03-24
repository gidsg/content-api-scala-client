package com.gu.openplatform.contentapi


import connection.{NingHttp, Http}
import java.net.URLEncoder
import com.gu.openplatform.contentapi.parser.JsonParser
import model._
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.ReadableInstant
import java.util.concurrent.Future


// thrown when an "expected" error is thrown by the api
case class ApiError(httpStatus: Int, httpMessage: String)
        extends Exception(httpMessage)


abstract class Api extends Http with JsonParser {
  val targetUrl = "http://content.guardianapis.com"
  var apiKey: Option[String] = None

  def sections = new SectionsQuery
  def tags = new TagsQuery
  def folders = new FoldersQuery
  def search = new SearchQuery
  def item = new ItemQuery

  class FoldersQuery
    extends GeneralParameters[FoldersQuery]
    with FilterParameters[FoldersQuery] {
    lazy val response: Future[FoldersResponse] = parseFolders(fetch(targetUrl + "/folders", parameters))
  }

  object FoldersQuery {
    implicit def asResponse(q: FoldersQuery) = q.response.get
    implicit def asSections(q: FoldersQuery) = q.response.get.results
  }


  class SectionsQuery
    extends GeneralParameters[SectionsQuery]
    with FilterParameters[SectionsQuery] {
    lazy val response: Future[SectionsResponse] = parseSections(fetch(targetUrl + "/sections", parameters))
  }

  object SectionsQuery {
    implicit def asResponse(q: SectionsQuery) = q.response.get
    implicit def asSections(q: SectionsQuery) = q.response.get.results
  }



  class TagsQuery extends GeneralParameters[TagsQuery]
          with PaginationParameters[TagsQuery]
          with FilterParameters[TagsQuery]
          with RefererenceParameters[TagsQuery]
          with ShowReferenceParameters[TagsQuery] {
    object tagType extends StringParameter(self, "type")
    lazy val response: Future[TagsResponse] = parseTags(fetch(targetUrl + "/tags", parameters))
  }

  object TagsQuery {
    implicit def asResponse(q: TagsQuery) = q.response.get
    implicit def asTags(q: TagsQuery) = q.response.get.results
  }



  class SearchQuery extends GeneralParameters[SearchQuery]
          with PaginationParameters[SearchQuery]
          with ShowParameters[SearchQuery]
          with RefinementParameters[SearchQuery]
          with FilterParameters[SearchQuery]
          with ContentFilterParameters[SearchQuery]
          with RefererenceParameters[SearchQuery]
          with ShowReferenceParameters[SearchQuery] {
    lazy val response: Future[SearchResponse] = parseSearch(fetch(targetUrl + "/search", parameters))
  }

  object SearchQuery {
    implicit def asResponse(q: SearchQuery) = q.response.get
    implicit def asContent(q: SearchQuery) = q.response.get.results
  }

  class ItemQuery extends GeneralParameters[ItemQuery]
          with ShowParameters[ItemQuery]
          with ContentFilterParameters[ItemQuery]
          with PaginationParameters[ItemQuery]
          with ShowReferenceParameters[ItemQuery] {
    var _apiUrl: Option[String] = None

    def apiUrl(newContentPath: String): this.type = {
      require(newContentPath startsWith targetUrl, "apiUrl expects a full url; use itemId if you only have an id")
      _apiUrl = Some(newContentPath)
      this
    }

    def itemId(contentId: String): this.type = apiUrl(targetUrl + "/" + contentId)

    lazy val response: Future[ItemResponse] = parseItem(
      fetch(
        _apiUrl.getOrElse(throw new Exception("No api url provided to item query, ensure withApiUrl is called")),
        parameters))
  }

  object ItemQuery {
    implicit def asResponse(q: ItemQuery) = q.response.get
  }



  trait GeneralParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    override def parameters = super.parameters ++ apiKey.map("api-key" -> _)
  }

  trait PaginationParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object pageSize extends IntParameter(self, "page-size")
    object page extends IntParameter(self, "page")
  }

  trait FilterParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object q extends StringParameter(self, "q")
    object section extends StringParameter(self, "section")
    object ids extends StringParameter(self, "ids")
    object tag extends StringParameter(self, "tag")
    object folder extends StringParameter(self, "folder")
  }

  trait ContentFilterParameters[OwnerType <: ParameterHolder] extends FilterParameters[OwnerType] {
    object orderBy extends StringParameter(self, "order-by")
    object fromDate extends DateParameter(self, "from-date")
    object toDate extends DateParameter(self, "to-date")
    object dateId extends StringParameter(self, "date-id")
    object useDate extends StringParameter(self, "use-date")
   }

  trait ShowParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object showFields extends StringParameter(self, "show-fields")
    object showSnippets extends StringParameter(self, "show-snippets")
    object showTags extends StringParameter(self, "show-tags")
    object showFactboxes extends StringParameter(self, "show-factboxes")
    object showMedia extends StringParameter(self, "show-media")
    object showRelated extends BoolParameter(self, "show-related")
    object showEditorsPicks extends BoolParameter(self, "show-editors-picks")
    object edition extends StringParameter(self, "edition")
    object showMostViewed extends BoolParameter(self, "show-most-viewed")
    object showStoryPackage extends BoolParameter(self, "show-story-package")
    object showBestBets extends BoolParameter(self, "show-best-bets")
    object snippetPre extends StringParameter(self, "snippet-pre")
    object snippetPost extends StringParameter(self, "snippet-post")
  }

  trait RefinementParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object showRefinements extends StringParameter(self, "show-refinements")
    object refinementSize extends IntParameter(self, "refinement-size")
  }

  trait RefererenceParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object reference extends StringParameter(self, "reference")
    object referenceType extends StringParameter(self, "reference-type")
  }

  trait ShowReferenceParameters[OwnerType <: ParameterHolder] extends Parameters[OwnerType] {
    object showReferences extends StringParameter(self, "show-references")
  }



  protected def fetch(url: String, parameters: Map[String, Any] = Map.empty): Future[String] = {
    require(!url.contains('?'), "must not specify parameters in url")

    def encodeParameter(p: Any): String = p match {
      case dt: ReadableInstant => URLEncoder.encode(ISODateTimeFormat.dateTimeNoMillis.print(dt), "UTF-8")
      case other => URLEncoder.encode(other.toString, "UTF-8")
    }

    val queryString = parameters.map {case (k, v) => k + "=" + encodeParameter(v)}.mkString("&")
    val target = url + "?" + queryString

    val responseFuture = GET(target, List("User-Agent" -> "scala-api-client", "Accept" -> "application/json"))

    wrapFuture(responseFuture){ response =>
      if (List(200, 302) contains response.statusCode) {
        response.body
      } else {
        throw new ApiError(response.statusCode, response.statusMessage)
      }
    }
  }
}

object Api extends Api with NingHttp