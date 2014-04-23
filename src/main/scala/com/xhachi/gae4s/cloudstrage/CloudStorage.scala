package com.xhachi.gae4s.cloudstrage

import com.google.appengine.tools.cloudstorage._
import java.nio.ByteBuffer
import java.io.StringReader
import scala.Some

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import com.xhachi.gae4s.common.Logger


object CloudStorage extends Logger {

  def apply(bucketName: String) = new CloudStorage(defaultService, bucketName)

  def defaultService = GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance)
}

class CloudStorage(service: GcsService, bucketName: String) extends Logger {

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

  def writeBytes(path: String, bytes: Array[Byte]) = writeByteBuffer(path, ByteBuffer.wrap(bytes))

  def writeByteBuffer(path: String, bytes: ByteBuffer) = {
    info("CloudStorage[" + bucketName + "] write : " + path)
    var c: GcsOutputChannel = null
    try {
      c = service.createOrReplace(path, GcsFileOptions.getDefaultInstance)
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

