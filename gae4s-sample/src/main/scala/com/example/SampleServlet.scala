package com.example

import java.util.Date
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import com.xhachi.gae4s.datastore._

class SampleServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val tx = Datastore.beginTx
    val key = Datastore.createKey("counter", 1)
    val entity = Datastore.getOption(key).getOrElse(new Entity(key, Seq(UnindexedProperty("count", 0), CreationDateProperty(new Date))))
    val counted = entity.set("count", entity[Long]("count") + 1L)
    Datastore.put(counted)
    tx.commit()

    val w = response.getWriter
    w.println(s"count: " + entity.get("count"))
    w.println(s"update: " + entity.get("createdAt"))
    w.flush()
  }

}
