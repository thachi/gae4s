package com.xhachi.gae4s.cloudstrage

import java.io.{OutputStream, OutputStreamWriter, StringReader}
import java.nio.ByteBuffer
import java.nio.channels.Channels

import com.google.appengine.tools.cloudstorage.GcsFileOptions.Builder
import com.google.appengine.tools.cloudstorage._
import com.sun.xml.internal.messaging.saaj.util.ByteInputStream
import com.xhachi.gae4s.cloudstrage.CloudStorage.MimeType
import com.xhachi.gae4s.common.Logger

import scala.xml.{Elem, XML}

object CloudStorage extends Logger {

  object MimeType {
    val Html = "text/html"
    val Text = "text/plain"
    val Xml = "text/xml"
    val Xhtml = "text/xhtml+xml"
    val JavaScript = "text/javascript"
    val Css = "text/css"

    val Json = "application/json"
    val Pdf = "application/pdf"

    val Jpeg = "image/jpeg"
    val Png = "image/png"
  }

  val Ext2MimeType = Map(
    ".html" -> MimeType.Html,
    ".txt" -> MimeType.Text,
    ".xml" -> MimeType.Xml,
    ".xhtml" -> MimeType.Xhtml,
    ".js" -> MimeType.JavaScript,
    ".css" -> MimeType.Css,
    ".json" -> MimeType.Json,
    ".pdf" -> MimeType.Pdf,
    ".jpeg" -> MimeType.Jpeg,
    ".jpg" -> MimeType.Jpeg,
    ".png" -> MimeType.Png
  )

  def apply(bucketName: String): CloudStorage = CloudStorage(defaultService, bucketName)

  def apply(service: GcsService, bucketName: String): CloudStorage = new CloudStorage(service, bucketName)

  def defaultService = GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance)
}

class CloudStorage private[cloudstrage](service: GcsService, bucketName: String) extends JsonOps with XMLOpt with Logger {


  def pathToFilename(path: String) = new GcsFilename(bucketName, path.replaceAll("^/+", ""))

  def metadata(path: String): Option[GcsFileMetadata] = {
    service.getMetadata(pathToFilename(path)) match {
      case m: GcsFileMetadata => Some(m)
      case _ => None
    }
  }

  def delete(path: String): Boolean = {
    info("CloudStorage[" + bucketName + "] delete : " + path)

    service.delete(pathToFilename(path))
  }

  def readByteBuffer(path: String): Option[ByteBuffer] = {
    metadata(path).map {
      case m =>
        info("CloudStorage[" + bucketName + "] read : " + path)
        val fileSize = m.getLength.toInt
        val result = ByteBuffer.allocate(fileSize)
        val c = service.openReadChannel(pathToFilename(path), 0)
        c.read(result)
        result
    }
  }

  def readBytes(path: String): Option[Array[Byte]] = readByteBuffer(path).map(_.array())

  def writeBytes(path: String, bytes: Array[Byte], mimeType: Option[String] = None, public: Boolean = false): Unit = {
    writeByteBuffer(path, ByteBuffer.wrap(bytes), mimeType, public)
  }

  def writeByteBuffer(path: String, bytes: ByteBuffer, mimeType: Option[String] = None, public: Boolean = false): Unit = {
    info("CloudStorage[" + bucketName + "] write : " + path)
    val option = toGcsFileOption(path, mimeType, public)
    var c: GcsOutputChannel = null
    try {
      c = service.createOrReplace(pathToFilename(path), option)
      c.write(bytes)
    } finally {
      if (c != null) c.close()
    }
  }

  def getOutputStream(path: String, mimeType: Option[String] = None, public: Boolean = false): OutputStream = {
    val option = toGcsFileOption(path, mimeType, public)
    val outputChannel = service.createOrReplace(pathToFilename(path), option)
    Channels.newOutputStream(outputChannel)
  }

  def toGcsFileOption(path: String, mimeType: Option[String], public: Boolean): GcsFileOptions = {
    val builder = new Builder()

    if (public) {
      builder.acl("public-read")
      builder.cacheControl("public; max-age=60")
    }

    mimeType match {
      case Some(m) =>
        builder.mimeType(m).build()
      case None =>
        CloudStorage.Ext2MimeType.find {
          case (e, m) => path.endsWith(e)
        }.map(_._2).map { m =>
          builder.mimeType(m).build()
        }.getOrElse {
          GcsFileOptions.getDefaultInstance
        }
    }
  }

}


sealed trait XMLOpt {


  def readXML(path: String): Option[Elem] = {
    readBytes(path) map (b => XML.load(new ByteInputStream(b, b.length)))
  }

  def writeXML(path: String, xml: Elem, public: Boolean = false): Unit = {
    val writer = new OutputStreamWriter(getOutputStream(path, Some(MimeType.Xml), public))
    try {
      XML.write(writer, xml, "UTF-8", xmlDecl = true, doctype = null)
    }
    finally {
      writer.flush()
      writer.close()
    }
  }

  def readBytes(path: String): Option[Array[Byte]]

  def getOutputStream(path: String, mimeType: Option[String] = None, public: Boolean = false): OutputStream

}

sealed  trait JsonOps {

  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.native.Serialization

  implicit var formats = DefaultFormats

  def readJson(path: String): Option[JValue] = {

    readBytes(path) map (b => parse(new StringReader(new String(b, "UTF-8"))))
  }

  def writeJson(path: String, value: JValue, public: Boolean = false): Unit = {

    val b = Serialization.write(value).getBytes("UTF-8")
    writeBytes(path, b, public = public)
  }

  def readBytes(path: String): Option[Array[Byte]]

  def writeBytes(path: String, bytes: Array[Byte], mimeType: Option[String] = None, public: Boolean = false): Unit

}

