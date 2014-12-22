package com.example

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import com.xhachi.gae4s.urlfetch.UrlFetch

class SampleServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val result = UrlFetch.get("http://google.com")
    println("done" + result.finalUrl)



  }

}
