package com.example

import java.util.Date

import com.xhachi.gae4s.buildinfo.BuildInfo
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import com.xhachi.gae4s.datastore._

class SampleServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val tx = Datastore.beginTx
    val key = Datastore.createKey("counter", 1)
    val entity = Datastore.getOption(key).getOrElse(
      Entity(
        key,
        Seq(
          UnindexedProperty[Long]("count", 0),
          IndexedProperty("createdAt", new Date),
          IndexedProperty("updatedAt", new Date)
        ))
    )
    val counted = entity
      .set("count", entity[Long]("count") + 1)
      .set("updatedAt", new Date)

    Datastore.put(counted)
    tx.commit()


    response.addHeader("Content-Type", "text/html")

    val w = response.getWriter
    w.println(s"count: " + entity.get("count"))
    w.println("<br />")
    w.println(s"createdAt: " + entity.get("createdAt"))
    w.println("<br />")
    w.println(s"updatedAt: " + entity.get("updatedAt"))
    w.println("<hr />")
    w.println(s"gae4s: " + BuildInfo.toString)
    w.flush()
  }

}
