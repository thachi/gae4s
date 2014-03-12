package com.xhachi.gae4s.tests

import org.scalatest.{Suite, BeforeAndAfterEach}
import java.util.logging.{Level, StreamHandler, Logger}
import com.google.appengine.tools.development.testing.{LocalServiceTestConfig, LocalServiceTestHelper}
import com.google.appengine.api.datastore.DatastoreServiceFactory

/**
 * Created by takashi on 2014/02/20.
 */
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

  def transaction[T](block: => T): T = {
    val service = DatastoreServiceFactory.getDatastoreService
    val tx = service.beginTransaction()
    val ret = try {
      val ret = block
      tx.commit()
      ret
    } finally {
      if (tx.isActive) tx.rollback()
    }
    ret
  }
}
