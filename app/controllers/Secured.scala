package controllers

import play.api._
import play.api.mvc._
import org.w3.banana.RDF

/**
 * Trait for user/password secured controllers
 *  cf https://github.com/playframework/playframework/blob/master/framework/src/play/src/main/scala/play/api/mvc/Security.scala
 */
trait Secured {

  val loginActivated = // false //
    true


  def username(request: RequestHeader) = request.session.get(Security.username)

  private def onUnauthorized(request: RequestHeader) =
    Results.Redirect(routes.Auth.authenticate)

  /** Ensures the controller is only accessible to registered users */
  private def withAuth[CT](b: BodyParser[CT])(fun: => String => Request[CT] => Result)
  : EssentialAction = {
    Security.Authenticated(username, onUnauthorized) {
      user =>
        val fun2 = request => fun(user)(request)
        Action[CT]( b )(fun2)
    }
  }
  
  /** Ensures authentication and passes the user to the controller */
  def withUser[CT](b: BodyParser[CT])(fun: String => Request[CT] => Result) =
    if(loginActivated)
      withAuth (b){ user_name =>
        implicit request: Request[CT] =>
            fun(user_name)(request)
      }
    else
      Action[CT](b){implicit request: Request[CT] => fun("anonyme")(request)}

}
