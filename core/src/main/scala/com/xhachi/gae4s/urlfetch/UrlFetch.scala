package com.xhachi.gae4s.urlfetch

import java.net.{URL, URLEncoder}

import com.google.appengine.api.urlfetch.HTTPMethod._
import com.google.appengine.api.urlfetch._

import scala.collection.JavaConversions._

/**
 * Class to access URL Fetch service.
 *
 * @author Takashi Hachinohe
 * @param service the URLFetchService instance
 * @param defaultHeaders the HTTP request Headers
 */
class UrlFetch private[UrlFetch](service: URLFetchService, defaultHeaders: Map[String, String], charset: String = "UTF-8") {

  class Response private[UrlFetch](response: HTTPResponse) {

    lazy val status: Int = response.getResponseCode

    lazy val content: Option[String] = response.getContent match {
      case content: Array[Byte] if content.nonEmpty => Some(new String(response.getContent, charset))
      case _ => None
    }

    lazy val finalUrl: String = response.getFinalUrl.toString

    lazy val headers: Map[String, String] = response.getHeaders.map(header => header.getName -> header.getValue).toMap
  }

  def get(url: String,
          query: Map[String, Seq[String]] = Map(),
          headers: Map[String, String] = Map()) = {
    fetch(createRequest(url, GET, query = query, headers = headers))
  }

  def head(url: String,
           query: Map[String, Seq[String]] = Map(),
           headers: Map[String, String] = Map()) = {
    fetch(createRequest(url, HEAD, query = query, headers = headers))
  }

  def post(url: String,
           query: Map[String, Seq[String]] = Map(),
           data: Map[String, Seq[String]] = Map(),
           headers: Map[String, String] = Map()) = {
    fetch(createRequest(url, POST, query = query, data = data, headers = headers))
  }

  def put(url: String,
          query: Map[String, Seq[String]] = Map(),
          data: Map[String, Seq[String]] = Map(),
          headers: Map[String, String] = Map()) = {
    fetch(createRequest(url, PUT, query = query, data = data, headers = headers))
  }

  def delete(url: String,
             query: Map[String, Seq[String]] = Map(),
             headers: Map[String, String] = Map()) = {
    fetch(createRequest(url, DELETE, query = query, headers = headers))
  }

  protected def fetch(request: HTTPRequest) = {
    new Response(service.fetch(request))
  }

  protected def createRequest(url: String,
                              method: HTTPMethod,
                              query: Map[String, Seq[String]] = Map(),
                              data: Map[String, Seq[String]] = Map(),
                              headers: Map[String, String] = Map()): HTTPRequest = {

    val requestUrl = if (query.isEmpty) {
      url
    } else {
      url + "?" + query.map {
        case (name, values) =>
          values.map {
            value =>
              URLEncoder.encode(name, charset) + "=" + URLEncoder.encode(value, charset)
          }
      }.flatten.mkString("&")
    }

    val request = new HTTPRequest(new URL(requestUrl), method)

    (defaultHeaders ++ headers).map {
      case (k, v) => new HTTPHeader(k, v)
    }.foreach(request.addHeader)

    if (data.nonEmpty) {
      val payload = data.map {
        case (name, values) => values.map {
          value => URLEncoder.encode(name, charset) + "=" + URLEncoder.encode(value, charset)
        }
      }.flatten
      request.setPayload(payload.mkString("&").getBytes(charset))
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
