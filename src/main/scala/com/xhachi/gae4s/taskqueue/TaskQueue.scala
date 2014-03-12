package com.xhachi.gae4s.taskqueue

import com.google.appengine.api.taskqueue.{TaskHandle, QueueFactory, TaskOptions, Queue}
import com.google.appengine.api.datastore.Transaction

import scala.collection.JavaConversions._

/**
 * Class to access Task Queue service.
 *
 * @author Takashi Hachinohe
 * @param queue the Queue instance
 */
class TaskQueue private[TaskQueue](val queue: Queue) {

  def add(task: TaskOptions) = queue.add(task)

  def addAll(tasks: Seq[TaskOptions]) = queue.add(tasks)

  def add(tx: Transaction, task: TaskOptions) = queue.add(tx, task)

  def addAll(tx: Transaction, tasks: Seq[TaskOptions]) = queue.add(tx, tasks)

  //  def addGet(tx: Transaction, task: TaskOptions) = queue.add(tx, task)

  def addWithoutTx(task: TaskOptions) = queue.add(null, task)

  def addAllWithoutTx(tasks: Seq[TaskOptions]) = queue.add(null, tasks)

  def delete(taskName: String) = queue.deleteTask(taskName)

  def delete(taskHandle: TaskHandle) = queue.deleteTask(taskHandle)

  def deleteAll(taskHandles: Seq[TaskHandle]) = queue.deleteTask(taskHandles)

  def queueName = queue.getQueueName

  def fetchStatistics = queue.fetchStatistics()

  def fetchStatistics(deadlineInSeconds: Double) = queue.fetchStatisticsAsync(deadlineInSeconds)
}

/**
 * Object to access default Task Queues service.
 *
 * @author Takashi Hachinohe
 */
object TaskQueue extends TaskQueue(QueueFactory.getDefaultQueue) {
  def apply(name: String) = new TaskQueue(QueueFactory.getQueue(name))
}