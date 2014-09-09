package com.xhachi.gae4s.cloudstrage

import java.io.StringReader
import java.nio.ByteBuffer

import com.google.appengine.tools.cloudstorage._
import com.xhachi.gae4s.common.Logger
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization


object CloudStorage extends Logger {

  def apply(bucketName: String) = new CloudStorage(defaultService, bucketName)

  def defaultService = GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance)
}

class CloudStorage(service: GcsService, bucketName: String) extends Logger {

  object MimeType {
    val HTML = "text/html"
    val TEXT = "text/plain"
    val XML = "text/xml"
    val XHTML = "text/xhtml+xml"
    val JavaScript = "text/javascript"
    val CSS = "text/css"

    val JSON = "application/json"
    val PDF = "application/pdf"

    val JPEG = "image/jpeg"
    val PNG = "image/png"
  }

  val Ext2MimeType = Map(
    ".html" -> MimeType.HTML,
    ".txt" -> MimeType.TEXT,
    ".xml" -> MimeType.XML,
    ".xhtml" -> MimeType.XHTML,
    ".js" -> MimeType.JavaScript,
    ".css" -> MimeType.CSS,
    ".json" -> MimeType.JSON,
    ".pdf" -> MimeType.PDF,
    ".jpeg" -> MimeType.JPEG,
    ".jpg" -> MimeType.JPEG,
    ".png" -> MimeType.PNG
  )

  info("CloudStorage[" + bucketName + "] created")

  implicit def pathToFilename(path: String) = new GcsFilename(bucketName, path)

  def metadata(path: String): Option[GcsFileMetadata] = {
    service.getMetadata(path) match {
      case m: GcsFileMetadata => Some(m)
      case _ => None
    }
  }

  def delete(path: String): Boolean = {
    info("CloudStorage[" + bucketName + "] delete : " + path)

    service.delete(path)
  }

  def readByteBuffer(path: String): Option[ByteBuffer] = {
    metadata(path) map {
      case m =>
        info("CloudStorage[" + bucketName + "] read : " + path)
        val fileSize = m.getLength.toInt
        val result = ByteBuffer.allocate(fileSize)
        val c = service.openReadChannel(path, 0)
        c.read(result)
        result
    }
  }

  def readBytes(path: String): Option[Array[Byte]] = readByteBuffer(path) map (_.array())

  def writeBytes(path: String, bytes: Array[Byte], mimeType: Option[String] = None) = {
    writeByteBuffer(path, ByteBuffer.wrap(bytes), mimeType)
  }

  def writeByteBuffer(path: String, bytes: ByteBuffer, mimeType: Option[String] = None) = {
    info("CloudStorage[" + bucketName + "] write : " + path)
    var c: GcsOutputChannel = null
    try {

      val option: GcsFileOptions = mimeType match {
        case Some(m) => new GcsFileOptions.Builder().mimeType(m).build()
        case None =>
          Ext2MimeType.map {
            case (e, m) => path.endsWith(e) match {
              case true => Some(m)
              case false => None
            }
          }.find(_.isDefined)
            .flatten
            .headOption match {
            case Some(m) => new GcsFileOptions.Builder().mimeType(m).build()
            case _ => GcsFileOptions.getDefaultInstance
          }

        case _ => GcsFileOptions.getDefaultInstance
      }

      c = service.createOrReplace(path, option)
      c.write(bytes)
    } catch {
      case e: Throwable => throw e
    } finally {
      if (c != null) c.close()
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

