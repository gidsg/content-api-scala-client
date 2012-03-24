package com.gu.openplatform.contentapi.connection

import java.lang.String
import com.gu.openplatform.contentapi._

import io.Codec
import java.util.concurrent.Future
import com.ning.http.client.{AsyncHttpClientConfig, RequestBuilder, AsyncHttpClient}

case class HttpResponse(body: String, statusCode: Int, statusMessage: String)

trait Http {
  implicit val codec = Codec("UTF-8")
  // this is what the Api client requires of an http connection
  def GET(url: String, headers: Iterable[ (String, String) ] = Nil): Future[HttpResponse]
}


trait NingHttp extends Http {
  
  val asyncHttpClient: AsyncHttpClient = NingHttp.asyncHttpClient

  def GET(urlString: String, headers: Iterable[ (String, String) ] = Nil): Future[HttpResponse] = {
    val requestBuilder = new RequestBuilder().setUrl(urlString)
    headers foreach { case (name, value) => requestBuilder.setHeader(name, value) }

    wrapFuture(asyncHttpClient.executeRequest(requestBuilder.build)) { response =>
      new HttpResponse(response.getResponseBody, response.getStatusCode, response.getStatusText)
    }
  }
}

object NingHttp {
  val config = new AsyncHttpClientConfig.Builder().setFollowRedirects(true).build
  val asyncHttpClient = new AsyncHttpClient(config);
}




