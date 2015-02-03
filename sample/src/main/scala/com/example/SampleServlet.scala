package com.example

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import com.xhachi.gae4s.datastore.{Datastore, Entity, Key, MutableEntity => MutableEntity}

class SampleServlet extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) = {

    val tx = Datastore.beginTx
    val key = Datastore.createKey[CounterEntity](1)
    val entity = Datastore.getOption(key).getOrElse(new CounterEntity(key))
    entity.countUp()
    Datastore.put(entity)
    tx.commit()

    val w = response.getWriter
    w.println(s"count: " + entity.count)
    w.println(s"update: " + entity.updatedAt)
    w.flush()
  }

}


class CounterEntity(val key: Key[CounterEntity],
                    var count: Long = 0)
  extends Entity[CounterEntity]
  with MutableEntity {

  def countUp() = count += 1
}