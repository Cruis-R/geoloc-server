package controllers

import play.api.mvc.Controller
import play.api.mvc.Action

object HTTP_Options extends Controller {
  
  val corsHeaders: Map[String, String] = Map(
    "Access-Control-Allow-Origin" -> "*",
    "Access-Control-Allow-Methods" -> "GET, POST",
    "Access-Control-Allow-Headers" -> "Link, Origin, Content-Type, Accept"
    // "Content-Type" -> "text/json; charset=utf-8, text/json-ld; charset=utf-8"
    )

  def httpOptions(path: String) = {
	  Action { implicit request =>
      println("OPTIONS: " + request)
      Ok("OPTIONS: " + request)
        .as("text/html; charset=utf-8")
        .withHeaders(corsHeaders.toList:_*)
    }
  }
}