package com.xhachi.gae4s.mail

import com.google.appengine.api.mail.MailService.{Attachment, Header, Message => LLMessage}
import com.google.appengine.api.mail.{MailService, MailServiceFactory}

/**
 * Object to access default Mail service.
 *
 * @author Takashi Hachinohe
 */
object Mail extends Mail(MailServiceFactory.getMailService)

/**
 * Class to access Mail service.
 *
 * @author Takashi Hachinohe
 * @param service the MailService instance
 */
class Mail(service: MailService) {

  /**
   * Send message.
   *
   * @param message Instance of Message
   */
  def send(message: Message) = service.send(message.toLLMessage)

  /**
   * Send message to Administrators
   *
   * @param message Instance of Message
   */
  def sendToAdmins(message: Message) = service.sendToAdmins(message.toLLMessage)

}

case class Message(
                    sender: String,
                    replyTo: Option[String] = None,
                    to: Seq[String] = Nil,
                    cc: Seq[String] = Nil,
                    bcc: Seq[String] = Nil,
                    subject: Option[String] = None,
                    textBody: Option[String] = None,
                    htmlBody: Option[String] = None,
                    attachments: Seq[Attachment] = Nil,
                    headers: Seq[Header] = Nil) {

  private[mail] def toLLMessage: LLMessage = {
    val m = new LLMessage()
    m.setSender(sender)
    replyTo.foreach(m.setReplyTo)
    if (to.nonEmpty) m.setTo(to: _*)
    if (cc.nonEmpty) m.setCc(cc: _*)
    if (bcc.nonEmpty) m.setBcc(bcc: _*)
    subject.foreach(m.setSubject)
    textBody.foreach(m.setTextBody)
    htmlBody.foreach(m.setHtmlBody)
    if (attachments.nonEmpty) m.setAttachments(attachments: _*)
    if (headers.nonEmpty) m.setHeaders(headers: _*)
    m
  }
}