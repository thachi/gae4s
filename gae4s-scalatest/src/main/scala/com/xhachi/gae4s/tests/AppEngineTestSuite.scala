package com.xhachi.gae4s.tests

import com.google.appengine.tools.development.testing.{LocalServiceTestConfig => AppEngoneLocalServiceTestConfig, _}
import org.scalatest.{BeforeAndAfterEach, Suite}

import scala.language.implicitConversions

trait AppEngineTestSuite extends BeforeAndAfterEach with LocalServiceTestConfig {
  self: Suite =>

  protected lazy val helper = new LocalServiceTestHelper(_localServiceTestConfigs: _*)


  override def beforeEach() = {
    super.beforeEach()
    helper.setUp()
  }

  override def afterEach() = {
    super.afterEach()
    helper.tearDown()
  }
}

trait LocalServiceTestConfig {
  protected def _localServiceTestConfigs: List[AppEngoneLocalServiceTestConfig] = Nil
}

object LocalServiceTestConfig {

  trait Datastore extends LocalServiceTestConfig {
    override def _localServiceTestConfigs = new LocalDatastoreServiceTestConfig :: super._localServiceTestConfigs
  }

  trait Mail extends LocalServiceTestConfig {
    override def _localServiceTestConfigs = new LocalMailServiceTestConfig :: super._localServiceTestConfigs
  }

  trait Memcache extends LocalServiceTestConfig {
    override def _localServiceTestConfigs = new LocalMemcacheServiceTestConfig :: super._localServiceTestConfigs
  }

  trait TaskQueue extends LocalServiceTestConfig {
    override def _localServiceTestConfigs = new LocalTaskQueueTestConfig :: super._localServiceTestConfigs
  }

  trait URLFetch extends LocalServiceTestConfig {
    override def _localServiceTestConfigs = new LocalURLFetchServiceTestConfig :: super._localServiceTestConfigs
  }

}