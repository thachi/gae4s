package com.xhachi.gae4s.tests

import org.scalatest.{Suite, BeforeAndAfterEach}
import java.util.logging.{Level, StreamHandler, Logger}
import com.google.appengine.tools.development.testing.{LocalServiceTestConfig, LocalServiceTestHelper}
import com.google.appengine.api.datastore.DatastoreServiceFactory
import com.xhachi.gae4s.datastore.{Key, EntityStoreContext}

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
