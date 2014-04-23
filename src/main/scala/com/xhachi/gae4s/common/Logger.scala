package com.xhachi.gae4s.common

import org.slf4j.LoggerFactory


trait Logger {

  lazy val logger = LoggerFactory.getLogger(this.getClass)

  def trace(message: => String): Unit = if (logger.isTraceEnabled) logger.trace(message)

  def debug(message: => String): Unit = if (logger.isDebugEnabled) logger.debug(message)

  def info(message: => String): Unit = if (logger.isInfoEnabled) logger.info(message)

  def warn(message: => String): Unit = if (logger.isWarnEnabled) logger.warn(message)

  def error(message: => String): Unit = if (logger.isErrorEnabled) logger.error(message)

  def warn(message: => String, e: Throwable): Unit = if (logger.isWarnEnabled) logger.warn(message, e)

  def error(message: => String, e: Throwable): Unit = if (logger.isErrorEnabled) logger.error(message, e)

}
