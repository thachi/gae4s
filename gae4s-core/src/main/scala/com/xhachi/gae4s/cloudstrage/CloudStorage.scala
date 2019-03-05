package com.xhachi.gae4s.cloudstrage

import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.util.Date

import com.google.appengine.tools.cloudstorage.GcsFileOptions.Builder
import com.google.appengine.tools.cloudstorage._
import com.xhachi.gae4s.cloudstrage.CloudStorage.MimeType
import com.xhachi.gae4s.common.Logger

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

  private val Ext2MimeType: Map[String, String] = Map(
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

  private val DefaultService = GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance)

  def apply(bucketName: String): CloudStorage = CloudStorage(DefaultService, bucketName)

  def apply(service: GcsService, bucketName: String): CloudStorage = new CloudStorage(service, bucketName)

}

case class Item(name: String, size: Long, lastModified: Date, directory: Boolean, etag: Option[String])

class CloudStorage private[cloudstrage](service: GcsService, bucketName: String)
  extends ReadOps
    with WriteOps
    with ListOps
    with Logger {

  private[cloudstrage] def pathToFilename(path: String) = new GcsFilename(bucketName, path.replaceAll("^/+", ""))

  def metadata(path: String): Option[GcsFileMetadata] = Option(service.getMetadata(pathToFilename(path)))

  def copy(source: String, dist: String): Unit = service.copy(pathToFilename(source), pathToFilename(dist))

  def list(options: ListOptions): Iterator[Item] = {
    val list = service.list(bucketName, options)

    new Iterator[ListItem] {

      override def hasNext: Boolean = list.hasNext

      override def next(): ListItem = list.next()

    }.map { i =>
      val tag = i.getEtag match {
        case t: String => Some(t)
        case _ => None
      }
      Item(i.getName, i.getLength, i.getLastModified, i.isDirectory, tag)
    }

  }

  def delete(path: String): Boolean = {
    info("CloudStorage[" + bucketName + "] delete : " + path)

    service.delete(pathToFilename(path))
  }

  def readByteBuffer(path: String): Option[ByteBuffer] = {
    metadata(path).map { m =>
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

  private[cloudstrage] def toGcsFileOption(path: String, mimeType: Option[String], public: Boolean): GcsFileOptions = {
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
          case (e, _) => path.endsWith(e)
        }.map(_._2).map { m =>
          builder.mimeType(m).build()
        }.getOrElse {
          GcsFileOptions.getDefaultInstance
        }
    }
  }

}

sealed trait WriteOps {

  def writeBytes(path: String, bytes: Array[Byte], mimeType: Option[String] = None, public: Boolean = false): Unit

  def writeText(path: String, text: String): Unit = writeBytes(path, text.getBytes("UTF-8"), Some(MimeType.Text))
}

sealed trait ReadOps {

  def readBytes(path: String): Option[Array[Byte]]

  def readText(path: String): Option[String] = readBytes(path).map(new String(_, "UTF-8"))
}

sealed trait ListOps {

  def list(options: ListOptions): Iterator[Item]

  def listAll: Iterator[Item] = list(ListOptions.DEFAULT)

  def listWithPrefix(prefix: String): Iterator[Item] = list {
    val b = new ListOptions.Builder
    b.setPrefix(prefix)
    b.build()
  }
}

