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
class UrlFetch(service: URLFetchService, defaultHeaders: Map[String, String], charset: String = "UTF-8") {

  class Response private[UrlFetch](response: HTTPResponse) {

    lazy val status: Int = response.getResponseCode

    lazy val content: Option[String] = response.getContent match {
      case content: Array[Byte] if !content.isEmpty => Some(new String(response.getContent, charset))
      case _ => None
    }

    lazy val finalUrl: String = response.getFinalUrl.toString

    lazy val headers: Map[String, String] = response.getHeaders.map(header => header.getName -> header.getValue).toMap
  }

  def get(url: String,
          query: Option[Map[String, Seq[String]]] = None,
          headers: Option[Map[String, String]] = None) = {
    fetch(createRequest(url, GET, query = query, headers = headers))
  }

  def head(url: String,
           query: Option[Map[String, Seq[String]]] = None,
           headers: Option[Map[String, String]] = None) = {
    fetch(createRequest(url, HEAD, query = query, headers = headers))
  }

  def post(url: String,
           query: Option[Map[String, Seq[String]]] = None,
           data: Option[Map[String, Seq[String]]] = None,
           headers: Option[Map[String, String]] = None) = {
    fetch(createRequest(url, POST, query = query, data = data, headers = headers))
  }

  def put(url: String,
          query: Option[Map[String, Seq[String]]] = None,
          data: Option[Map[String, Seq[String]]] = None,
          headers: Option[Map[String, String]] = None) = {
    fetch(createRequest(url, PUT, query = query, data = data, headers = headers))
  }

  def delete(url: String,
             query: Option[Map[String, Seq[String]]] = None,
             headers: Option[Map[String, String]] = None) = {
    fetch(createRequest(url, DELETE, query = query, headers = headers))
  }

  protected def fetch(request: HTTPRequest) = {
    new Response(service.fetch(request))
  }

  protected def createRequest(url: String,
                              method: HTTPMethod,
                              query: Option[Map[String, Seq[String]]] = None,
                              data: Option[Map[String, Seq[String]]] = None,
                              headers: Option[Map[String, String]] = None): HTTPRequest = {

    val requestUrl = query match {
      case Some(d) =>
        val queryString = d.map {
          case (key, values) => values.map {
            value => URLEncoder.encode(key, charset) + "=" + URLEncoder.encode(value, charset)
          }
        }.flatten

        if (0 < queryString.size) url + "?" + queryString.mkString("&")
        else url
      case None => url
    }

    val request: HTTPRequest = new HTTPRequest(new URL(requestUrl), method)

    (defaultHeaders ++ headers.getOrElse(Nil)).map {
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
