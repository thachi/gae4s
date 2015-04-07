package com.xhachi.gae4s.cloudstrage

import java.io.{OutputStream, StringReader}
import java.nio.ByteBuffer
import java.nio.channels.Channels

import com.google.appengine.tools.cloudstorage.GcsFileOptions.Builder
import com.google.appengine.tools.cloudstorage._
import com.xhachi.gae4s.common.Logger
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization

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

class CloudStorage private[cloudstrage](service: GcsService, bucketName: String) extends Logger {


  def pathToFilename(path: String) = new GcsFilename(bucketName, path)

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

  def writeBytes(path: String, bytes: Array[Byte], mimeType: Option[String] = None) = {
    writeByteBuffer(path, ByteBuffer.wrap(bytes), mimeType)
  }

  def writeByteBuffer(path: String, bytes: ByteBuffer, mimeType: Option[String] = None) = {
    info("CloudStorage[" + bucketName + "] write : " + path)
    val option = toGcsFileOption(path, mimeType)
    var c: GcsOutputChannel = null
    try {
      c = service.createOrReplace(pathToFilename(path), option)
      c.write(bytes)
    } catch {
      case e: Throwable => throw e
    } finally {
      if (c != null) c.close()
    }
  }

  def getOutputStream(path: String, mimeType: Option[String] = None): OutputStream = {
    val option = toGcsFileOption(path, mimeType)
    val outputChannel = service.createOrReplace(pathToFilename(path), option)
    Channels.newOutputStream(outputChannel)
  }

  def toGcsFileOption(path: String, mimeType: Option[String]): GcsFileOptions = mimeType match {
    case Some(m) =>
      new Builder().mimeType(m).build()
    case None =>
      CloudStorage.Ext2MimeType.find {
        case (e, m) => path.endsWith(e)
      }.map(_._2).map { m =>
        new Builder().mimeType(m).build()
      }.getOrElse {
        GcsFileOptions.getDefaultInstance
      }
  }

  implicit var formats = DefaultFormats

  def readJson(path: String): Option[JValue] = {
    readBytes(path) map (b => parse(new StringReader(new String(b, "UTF-8"))))
  }

  def writeJson(path: String, value: JValue): Unit = {
    val b = Serialization.write(value).getBytes("UTF-8")
    writeBytes(path, b)
  }

}

