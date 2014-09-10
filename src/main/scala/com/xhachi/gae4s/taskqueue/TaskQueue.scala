package com.xhachi.gae4s.taskqueue

import java.util.concurrent.Future

import com.google.appengine.api.datastore.Transaction
import com.google.appengine.api.taskqueue._

import scala.collection.JavaConverters._

/**
 * Class to access Task Queue service.
 *
 * @author Takashi Hachinohe
 * @param queue the Queue instance
 */
class TaskQueue private[TaskQueue](val queue: Queue) {

  def add(task: TaskOptions): TaskHandle = queue.add(task)

  def addAll(tasks: Seq[TaskOptions]): Seq[TaskHandle] = queue.add(tasks.asJava).asScala

  def add(tx: Transaction, task: TaskOptions):TaskHandle = queue.add(tx, task)

  def addAll(tx: Transaction, tasks: Seq[TaskOptions]): Seq[TaskHandle] = queue.add(tx, tasks.asJava).asScala

  //  def addGet(tx: Transaction, task: TaskOptions) = queue.add(tx, task)

  def addWithoutTx(task: TaskOptions):TaskHandle = queue.add(null, task)

  def addAllWithoutTx(tasks: Seq[TaskOptions]):Seq[TaskHandle] = queue.add(null, tasks.asJava).asScala

  def delete(taskName: String):Boolean = queue.deleteTask(taskName)

  def delete(taskHandle: TaskHandle):Boolean = queue.deleteTask(taskHandle)

  def deleteAll(taskHandles: Seq[TaskHandle]): Seq[Boolean] = queue.deleteTask(taskHandles.asJava).asScala.toSeq.map(_ == true)

  def queueName: String = queue.getQueueName

  def fetchStatistics: QueueStatistics = queue.fetchStatistics()

  def fetchStatistics(deadlineInSeconds: Double): Future[QueueStatistics] = queue.fetchStatisticsAsync(deadlineInSeconds)
}

/**
 * Object to access default Task Queues service.
 *
 * @author Takashi Hachinohe
 */
object TaskQueue extends TaskQueue(QueueFactory.getDefaultQueue) {
  def apply(name: String) = new TaskQueue(QueueFactory.getQueue(name))
}