package com.xhachi.gae4s.urlfetch

import com.google.appengine.api.urlfetch._
import java.net.{URLEncoder, URL}

import com.google.appengine.api.urlfetch.HTTPMethod._

import scala.collection.JavaConversions._

/**
 * Class to access URL Fetch service.
 *
 * @author Takashi Hachinohe
 * @param service the URLFetchService instance
 * @param headers the HTTP request Headers
 */
class UrlFetch(service: URLFetchService, headers: Map[String, String], charset: String = "UTF-8") {

  class Response private[UrlFetch](response: HTTPResponse) {

    lazy val status: Int = response.getResponseCode

    lazy val content: Option[String] = response.getContent match {
      case content: Array[Byte] if !content.isEmpty => Some(new String(response.getContent, charset))
      case _ => None
    }

    lazy val finalUrl: String = response.getFinalUrl.toString

    lazy val headers: Map[String, String] = response.getHeaders.map(header => header.getName -> header.getValue).toMap
  }

  def get(url: String) = fetch(createRequest(url, GET))

  def head(url: String) = fetch(createRequest(url, HEAD))

  def post(url: String, data: Map[String, Seq[String]]) = fetch(createRequest(url, POST, Some(data)))

  def put(url: String, data: Map[String, Seq[String]]) = fetch(createRequest(url, PUT, Some(data)))

  def delete(url: String) = fetch(createRequest(url, POST))

  protected def fetch(request: HTTPRequest) = {
    new Response(service.fetch(request))
  }

  protected def createRequest(url: String, method: HTTPMethod, data: Option[Map[String, Seq[String]]] = None): HTTPRequest = {
    val request: HTTPRequest = new HTTPRequest(new URL(url), method)

    headers.map {
      case (k, v) => new HTTPHeader(k, v)
    }.foreach(request.addHeader)

    data match {
      case Some(d) =>
        val payload = d.map {
          case (key, values) => values.map {
            value => URLEncoder.encode(key, charset) + "=" + URLEncoder.encode(value, charset)
          }
        }.flatten
        request.setPayload(payload.mkString("&").getBytes(charset))
      case None => // no-op
    }
    request
  }
}

/**
 * Object to access default URL Fetch service.
 *
 * @author Takashi Hachinohe
 */
object UrlFetch extends UrlFetch(URLFetchServiceFactory.getURLFetchService, Map(), "UTF-8") {
  def apply(headers: Map[String, String], charset: String = "UTF-8") = {
    new UrlFetch(URLFetchServiceFactory.getURLFetchService, headers, charset)
  }
}
