package com.xhachi.gae4s.cloudstrage

import com.xhachi.gae4s.tests.{AppEngineTestSuite, LocalServiceTestConfig}
import org.scalatest.FunSuite

class CloudStorageTest extends FunSuite
  with AppEngineTestSuite
  with LocalServiceTestConfig.Datastore {

  val target: CloudStorage = CloudStorage("test")

  test("とりあえず動かしてみる") {
    val s = "test"
    target.writeBytes("path/using", s.getBytes)
    val actual = target.readBytes("path/using")

    assert(actual.isDefined)
    assert(new String(actual.get, "UTF-8") == s)
  }

  test("先頭に/がある場合にパス名が正しく取得できること") {
    assert(target.pathToFilename("/path/using").getObjectName == "path/using")
  }

  test("ファイルがない場合") {
    assert(target.readBytes("path/using").isEmpty)
  }

  test("ファイルがある場合のmetadata") {
    val now = System.currentTimeMillis()
    Thread.sleep(1)

    val s = "test"
    target.writeBytes("path/metadata", s.getBytes)
    val m = target.metadata("path/metadata")


    assert(m.isDefined)
    //    assert(m.get.getEtag == "")
    assert(m.get.getLastModified.getTime > now)
    assert(m.get.getLength == 4)
    assert(m.get.getFilename.getBucketName == "test")
    assert(m.get.getFilename.getObjectName == "path/metadata")
  }

  test("ファイルがない場合のmetadata") {
    assert(target.metadata("path/using").isEmpty)
  }

  test("ファイルがcopyできること") {
    target.writeText("source.txt", "123")
    target.copy("source.txt", "dist.txt")

    val actual = target.readText("dist.txt")

    assert(actual.isDefined)
    assert(actual.get == "123")
  }

  test("初期状態でlistAllが空であること") {
    val actual = target.listAll
    assert(actual.isEmpty)
  }

  test("ファイルを保存してlistAllで情報が取得できること") {
    val start = System.currentTimeMillis()
    Thread.sleep(1)
    target.writeText("sample.txt", "hoge")

    val actual = target.listAll
    assert(actual.nonEmpty)
    assert(actual.head.name == "sample.txt")
    assert(actual.head.size == 4)
    assert(actual.head.lastModified.getTime > start)
    assert(!actual.head.directory)
    assert(actual.head.etag.isEmpty)
    assert(actual.size == 1)
  }

  test("ファイルを複数保存してlistAllで情報が取得できること") {
    target.writeText("a.txt", "1")
    target.writeText("b.txt", "2")

    val actual = target.listAll
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("a.txt", "b.txt"))
  }

  test("ファイルを複数保存してlistAllで名前順で情報が取得できること") {
    target.writeText("b.txt", "2")
    target.writeText("a.txt", "1")

    val actual = target.listAll
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("a.txt", "b.txt"))
  }

  test("ディレクトリ内にファイルを保存してlistAllで情報が取得できること") {
    target.writeText("dir1/a.txt", "1")
    target.writeText("dir1/b.txt", "2")
    target.writeText("dir2/c.txt", "2")

    val actual = target.listAll
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("dir1/a.txt", "dir1/b.txt", "dir2/c.txt"))
  }

  test("ディレクトリ内にファイルを保存してlistAllで名前順で情報が取得できること") {
    target.writeText("dir1/a.txt", "1")
    target.writeText("dir2/b.txt", "2")
    target.writeText("dir1/c.txt", "3")

    val actual = target.listAll
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("dir1/a.txt", "dir1/c.txt", "dir2/b.txt"))
  }

  test("listWithPrefixで情報が取得できること1") {
    target.writeText("dir/a.txt", "1")
    target.writeText("dir1/b.txt", "2")
    target.writeText("dir2/dir/c.txt", "3")

    val actual = target.listWithPrefix("dir")
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("dir/a.txt", "dir1/b.txt", "dir2/dir/c.txt"))
  }

  test("listWithPrefixで情報が取得できること2") {
    target.writeText("dir/a.txt", "1")
    target.writeText("dir1/b.txt", "2")
    target.writeText("dir2/dir/c.txt", "3")

    val actual = target.listWithPrefix("dir/")
    assert(actual.nonEmpty)
    assert(actual.map(_.name) == Stream("dir/a.txt"))
  }

}
