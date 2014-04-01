package com.xhachi.gae4s.taskqueue

import org.scalatest.FunSuite
import com.google.appengine.tools.development.testing.{LocalTaskQueueTestConfig, LocalDatastoreServiceTestConfig}
import com.google.appengine.api.taskqueue.{TaskHandle, TransactionalTaskException, TaskOptions}
import com.xhachi.gae4s.tests.AppEngineTestSuite
import com.xhachi.gae4s.datastore.Datastore

class TaskQueueTest extends FunSuite with AppEngineTestSuite {

  override def getConfig = new LocalDatastoreServiceTestConfig :: new LocalTaskQueueTestConfig :: super.getConfig

  test("TaskQueueのデフォルトの名称が取得できること") {
    val name = TaskQueue.queueName
    assert(name == "default")
  }

  test("TaskQueueの名称が取得できること") {
    val name = TaskQueue("queue1").queueName
    assert(name == "queue1")
  }

  test("TaskQueueにaddできること") {
    TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
  }

  test("TaskQueueにaddしてdeleteできること") {
    val handle = TaskQueue.add(TaskOptions.Builder.withUrl("/task/null").countdownMillis(10000))
    val actual = TaskQueue.delete(handle)
    assert(actual)
  }

  test("TaskQueueにトランザクション内でaddしてdeleteできないこと") {
    Datastore.tx {
      val handle = TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
      val actual = TaskQueue.delete(handle)
      assert(!actual)
    }
  }

  test("TaskQueueにトランザクション内でaddしてトランザクション外でdeleteできること") {
    val handle: TaskHandle = Datastore.tx {
      TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
    }
    val actual = TaskQueue.delete(handle)
    assert(actual)
  }

  test("TaskQueueの統計情報を取得できること") {
    TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
    val statistics = TaskQueue.fetchStatistics
    assert(statistics != null)
  }

  test("TaskQueueにトランザクション内で5回addできること") {
    Datastore.tx {
      1 to 5 foreach {
        _ => TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
      }
    }
  }

  test("TaskQueueにトランザクション内で6回addしたらエラーになること") {
    Datastore.tx {
      1 to 5 foreach {
        _ => TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
      }
      intercept[TransactionalTaskException] {
        TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
      }
    }
  }

  test("TaskQueue#addWithoutTxがトランザクションから除外されていること") {
    Datastore.tx {
      TaskQueue.addWithoutTx(TaskOptions.Builder.withUrl("/task/null"))
      1 to 5 foreach {
        _ => TaskQueue.add(TaskOptions.Builder.withUrl("/task/null"))
      }
    }
  }
}