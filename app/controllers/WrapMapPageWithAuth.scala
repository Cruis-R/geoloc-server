package controllers

import play.api.mvc.{Action, Controller}
import play.api.mvc.Request
import play.api.mvc.AnyContent

object WrapMapPageWithAuth extends Controller with Secured {
 
  /** emballer la page principale (carte) pour qu'elle soit protégée par mot de passe */
  def show() =
		withUser[AnyContent](parse.anyContent) {
	    implicit userid: String =>
      implicit request: Request[AnyContent] =>
        Redirect( "/assets/phileas/index.html" )
  }
}