package com.xhachi.gae4s.tests

import java.util.logging.{Level, Logger, StreamHandler}

import com.google.appengine.tools.development.testing.{LocalServiceTestConfig, LocalServiceTestHelper}
import com.xhachi.gae4s.datastore.{EntityStoreContext, Key}
import org.scalatest.{BeforeAndAfterEach, Suite}

import scala.language.implicitConversions

trait AppEngineTestSuite extends BeforeAndAfterEach {

  this: Suite =>

  val logger = Logger.getLogger("com.google")
  logger.addHandler(new StreamHandler() {
    {
      setOutputStream(System.out)
      setLevel(Level.OFF)
    }
  })

  lazy val helper = new LocalServiceTestHelper(getConfig: _*)

  def getConfig: List[LocalServiceTestConfig] = Nil

  override def beforeEach() = helper.setUp()

  override def afterEach() = helper.tearDown()

  implicit def keyToContext(key: Key[_]) = new EntityStoreContext {
    override def ancestor: Option[Key[_]] = Some(key)
  }

}
