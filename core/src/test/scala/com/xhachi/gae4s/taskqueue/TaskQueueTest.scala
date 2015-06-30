package com.xhachi.gae4s.taskqueue

import com.google.appengine.api.taskqueue.{TaskHandle, TaskOptions, TransactionalTaskException}
import com.google.appengine.tools.development.testing.{LocalDatastoreServiceTestConfig, LocalTaskQueueTestConfig}
import com.xhachi.gae4s.datastore.Datastore
import com.xhachi.gae4s.tests.AppEngineTestSuite
import org.scalatest.FunSuite

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

  test("登録してないタスクを削除すると失敗すること") {
    assert(!TaskQueue.delete("hoge"))
  }

  test("名前付きのタスクを登録して削除すると成功すること") {
    TaskQueue.add(TaskOptions.Builder.withUrl("/task/null").taskName("hoge"))

    assert(TaskQueue.delete("hoge"))
  }
}