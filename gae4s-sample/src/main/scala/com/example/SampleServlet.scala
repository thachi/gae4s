package com.example

import java.util.Date

import com.xhachi.gae4s.buildinfo.BuildInfo
import com.xhachi.gae4s.common.AppInfo
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import com.xhachi.gae4s.datastore._

class SampleServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val key = Datastore.tx { _=>
      val key = Datastore.createKey("counter", 1)
      val entity = Datastore.getOption(key).getOrElse(
        Entity(
          key,
          Seq(
            UnindexedProperty[Long]("count", 0),
            IndexedProperty("createdAt", new Date),
            IndexedProperty("updatedAt", new Date),
            VersionProperty("version", 0L)
          ))
      )
      val counted = entity
        .set("count", entity[Long]("count") + 1)
        .set("updatedAt", new Date)
        .versioned("version")

      Datastore.put(counted)

      key
    }

    val stored = Datastore.get(key)


    response.addHeader("Content-Type", "text/plain")

    val w = response.getWriter
    w.println(s"count: " + stored.get("count"))
    w.println(s"version: " + stored.get("version"))
    w.println(s"createdAt: " + stored.get("createdAt"))
    w.println(s"updatedAt: " + stored.get("updatedAt"))
    w.println("----------")
    w.println(s"gae4s: " + BuildInfo.toString)
    w.println(s"app.id: " + AppInfo.idOption)
    w.println(s"app.version: " + AppInfo.versionOption)
    w.println(s"app.environment: " + AppInfo.environmentOption)
    w.flush()
  }

}
