package controllers

import java.io.PrintWriter

trait DEBUGMessages {
  val doRrecordMessages = true
  var messageCount = 0

  def recordMessage(messageType: String, result: String): Unit = {
    if (doRrecordMessages) {
      val fileName = messageType + messageCount + ".log.txt"
      val pw = new PrintWriter(fileName)
      pw.append(result)
      pw.close()
      messageCount = messageCount + 1
    }
  }

}