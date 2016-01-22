package com.xhachi.gae4s.concurrent

import java.util.concurrent.{Future => JFuture}


object Future {
  def apply[A](future: JFuture[A]) = new Future[A] {
    def get: A = future.get
  }
}

trait Future[A] {
  def get: A

  def map[B](f: A => B) = new Future[B] {
    def get: B = f(Future.this.get)
  }

  def flatMap[B](f: A => Future[B]): Future[B] = new Future[B] {
    def get: B = f(Future.this.get).get
  }
}

