package com.xhachi.gae4s.concurrent

import java.util.concurrent.{Future => JFuture}


object Future {
  def apply[A](future: JFuture[A]) = new FutureWrapper[A](future)
}

trait Future[A] {
  def get: A

  def map[B](f: A => B) = new Future[B] {
    def get: B = f(Future.this.get)
  }

  def flatMap[B](f: A => Future[B]) = new Future[B] {
    def get: B = f(Future.this.get).get
  }
}

class FutureWrapper[A](future: JFuture[A]) extends Future[A] {

  def get: A = future.get()

}
