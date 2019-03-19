package com.xhachi.gae4s.common

import java.util.logging.{Level, Logger => JLogger}

trait Logger {

  private val logger: JLogger = JLogger.getLogger(this.getClass.toString)

  def trace(message: => String): Unit = if (logger.isLoggable(Level.FINER)) logger.finer(message)

  def debug(message: => String): Unit = if (logger.isLoggable(Level.FINE)) logger.fine(message)

  def info(message: => String): Unit = if (logger.isLoggable(Level.INFO)) logger.info(message)

  def warn(message: => String): Unit = if (logger.isLoggable(Level.WARNING)) logger.warning(message)

  def error(message: => String): Unit = if (logger.isLoggable(Level.SEVERE)) logger.severe(message)

  def warn(message: => String, e: Throwable): Unit = if (logger.isLoggable(Level.WARNING)) {
    logger.warning(message + "\n" + e.getStackTrace.map(_.toString).mkString("\n"))
  }

  def error(message: => String, e: Throwable): Unit = if (logger.isLoggable(Level.SEVERE)) {
    logger.severe(message + "\n" + e.getStackTrace.map(_.toString).mkString("\n"))
  }

}

object Logger extends Logger