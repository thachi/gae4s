package com.example

import com.xhachi.gae4s.buildinfo.BuildInfo
import com.xhachi.gae4s.common.AppInfo
import javax.servlet.annotation.WebServlet
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

@WebServlet(Array("/"))
class IndexServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = {

    response.addHeader("Content-Type", "text/plain")

    val w = response.getWriter
    w.println(s"gae4s: " + BuildInfo.toString)
    w.println(s"app.id: " + AppInfo.idOption)
    w.println(s"app.version: " + AppInfo.versionOption)
    w.println(s"app.environment: " + AppInfo.environmentOption)
    w.flush()
  }

}
