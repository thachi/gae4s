package com.xhachi.gae4s.mail

import com.google.appengine.api.mail.MailService
import com.xhachi.gae4s.tests.{AppEngineTestSuite, LocalServiceTestConfig}
import org.scalatest.FunSuite


class MailTest
  extends FunSuite
    with AppEngineTestSuite
    with LocalServiceTestConfig.Mail {

  test("送信してみる") {
    Mail.send(Message(
      sender = "dev@xhachiapps.com",
      to = "dev@xhachiapps.com" :: Nil,
      subject = Some("test message from MailTest"))
    )
  }

  test("中身を確認してみる") {

    val message = Message(
      sender = "dev@xhachiapps.com",
      to = "dev@xhachiapps.com" :: Nil,
      subject = Some("test message from MailTest"))

    val mail = new Mail(new MailService {
      override def send(m: MailService.Message): Unit = {
        assert(m.getSender == message.sender)
        assert(m.getTo.size() == 1)
        assert(m.getTo.contains(message.to(0)))
        assert(m.getSubject == message.subject.get)

        assert(m.getCc == null)
        assert(m.getBcc == null)
      }

      override def sendToAdmins(p1: MailService.Message): Unit = ???
    })

    mail.send(message)
  }

}
