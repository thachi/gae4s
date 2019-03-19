package com.example

import com.xhachi.gae4s.buildinfo.BuildInfo
import com.xhachi.gae4s.common.AppInfo
import com.xhachi.gae4s.memcache.Memcache
import javax.servlet.annotation.WebServlet
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

@WebServlet(Array("/memcache"))
class MemcacheServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val count = Memcache.inclement("memcache_counter", 1L, Some(0L))

    response.addHeader("Content-Type", "text/plain")

    val w = response.getWriter
    w.println(s"count: " + count)
    w.println("----------")
    w.println(s"gae4s: " + BuildInfo.toString)
    w.println(s"app.id: " + AppInfo.idOption)
    w.println(s"app.version: " + AppInfo.versionOption)
    w.println(s"app.environment: " + AppInfo.environmentOption)
    w.flush()
  }

}
