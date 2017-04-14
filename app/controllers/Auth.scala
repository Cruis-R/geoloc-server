package controllers

import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

import org.apache.jena.query.Dataset

import models.driver.DriversDB
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Controller
import play.api.mvc.Request
import play.api.mvc.Security

object Auth extends JenaModule
with Auth[Jena, Dataset] {
  println(s"object Auth")
}

/** Controller for login, logout;
 *  see https://www.playframework.com/documentation/2.4.x/ScalaSessionFlash */
trait Auth[Rdf <: RDF, DATASET]
		extends Controller with JSONUtils {

  val redirect = true
  val checkPassword = false // for tests

  /** start a session if user Id & password are OK */
  def authenticate = Action { implicit request =>
    val userUri_password = bindFromRequest
    val userUri = userUri_password._1
    val userOK = !checkPassword ||
      DriversDB.checkPassword(userUri, userUri_password._2)
    userOK match {
      case true =>
        println(s"authenticate: checkPassword: pwd Match")
        if (redirect) {
          println(s"""Redirect( "/assets/phileas/index.html?@id=" + $userUri )""")
          val name = DriversDB.findFOAFname(userUri).getOrElse("???")
          Redirect("/assets/phileas/index.html?@id=" + userUri +
            s"&name=$name")
            .withSession(Security.username -> userUri)
        } else {
          Ok(wrapInJSONLD(
            makeJSONTerm("@id", userUri) + ",\n" +
              makeJSONTerm("name", DriversDB.findFOAFname(userUri).getOrElse("sans nom"))))
            .as("text/json; charset=utf-8")
        }
      case false =>
        println(s"authenticate: checkPassword: user '$userUri' not OK")
        Redirect("/assets/phileas/authenticate.html")
    }
  }

  /**
   * Le formulaire de login envoie /authenticate?@id=bla?password=secret ,
   * ou bien  envoie un JSON simple avec "@id" , "password"
-   * ensuite l'infrastructure Play! va gérer la session.
   */
  def bindFromRequest(implicit request: Request[AnyContent]): (String, String) = {
    val jsonOpt = request.body.asJson
    println(s"bindFromRequest: jsonOpt: $jsonOpt")
    jsonOpt match {
      case Some(json) =>
        println(s"bindFromRequest: request: $request")
        println(s"bindFromRequest: request.rawQueryString: ${request.rawQueryString}")
        println(s"bindFromRequest: request: ${json}")
        (
          (json \ "@id").as[String] ,
          (json \ "password").as[String]
        )
      case None =>
        println(s"bindFromRequest: no JSON sent! $request , trying HTTP request parameters")
        val id = request.getQueryString("@id").getOrElse("")
        val password = request.getQueryString("password").getOrElse("")
        if ( id == "" ) {
        	println("bindFromRequest: body " + request.body )
//        	val body = request.body.asFormUrlEncoded
        	val body = request.body.asMultipartFormData
        	println("bindFromRequest: " + body )
//        	( body.get("@id").head, body.get("password").head )
        	println("bindFromRequest: no expected HTTP request parameters, POST content not implemented." )
        	( "", "" )
        } else
          ( id, password )
    }
  }

  def logout = Action {
    Redirect(routes.Auth.authenticate).withNewSession.flashing(
      "success" -> "You are now logged out."
    )
  }
}
