package controllers

import java.util.Date

import org.w3.banana.RDF

import models.MessageStackRDF
import models.geo.GeoDataJSON_LDTrait
import models.geo.GeoDataTrait
import models.geo.PointData
import models.geo.RideDBTrait
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.mvc.Controller
import play.api.mvc.Request
import play.api.libs.json.JsLookupResult
import play.api.libs.json.JsString

/** Service pour tracer la Position de mobiles géolocalisés;
 * reçoit un JSON avec un @id et une position, renvoie
 * la même chose pour tous les @id connus, sauf ceux qui ont cessé d'émettre depuis 40s.
 *
 * NOTE: pas de dépendance à Archery
 * */
trait PositionService[Rdf <: RDF, DATASET] extends Controller with Secured
with JSONUtils
with RideDBTrait[Rdf, DATASET] 
with GeoDataJSON_LDTrait[Rdf]
with DEBUGMessages {
  
  val serviceName = "driver"

  val geoData: GeoDataTrait
  val messageStack: MessageStackRDF[Rdf]
  import ops._

  /**
   * Un message typique du téléphone vers le serveur en N-Triples:
   * <urn:user/u1> <http://www.w3.org/2003/01/geo/wgs84_pos#lat>  "48.862415" .
   * <urn:user/u1> <http://www.w3.org/2003/01/geo/wgs84_pos#long>  "2.342431" .
   *
   * Le même en JSON-LD (via convertisseur http://rdf-translator.appspot.com/ ):
   * {
   * "@context": {
   *   "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
   *   "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
   *   "xsd": "http://www.w3.org/2001/XMLSchema#"
   * },
   * "@id": "urn:user/u1",
   * "http://www.w3.org/2003/01/geo/wgs84_pos#lat": "48.862415",
   * "http://www.w3.org/2003/01/geo/wgs84_pos#long": "2.342431"
   * }
   *
   * Le même en JSON-LD avec un @context hébergé sur le Web:
   * {
   *   "@context": "https://deductions.github.io/drivers.context.jsonld",
   *   "@id": "urn:user/u1",
   *   "lat": "48.862415",
   *   "long": "2.342431"
   * }
   * 
   * Pour tester: script test/scenario_nominal.sh
   */
  def position =
    withUser (parse.json) {
	    implicit userid =>
      implicit request: Request[JsValue] =>
        computeResult(request)
  }

  private def computeResult(request: Request[JsValue]) = {
    val json = request.body
    // cf https://www.playframework.com/documentation/2.4.x/ScalaJson
    val jsonString = json.toString()
    val currentTimestamp = new Date()
    println(s"\nPositionService.computeResult; received JSON at ${currentTimestamp.toString()} : ${jsonString}")
    recordMessage( serviceName + "-received", jsonString)

    /* updateSender Point In 2D Database @return ID of received Point */
    def updateSenderPointIn2D_Database(): String = {
      // test: not an empty JSON:
      if (jsonString.contains("\"")) {
        val receivedPointData = PointData(
          (json \ "@id").as[String], currentTimestamp.getTime)
          val lat = getFloat(json \ "lat")
          val long = getFloat(json \ "long")
        println(s"""Gotten by HTTP: ${currentTimestamp.toString()}
        @id ${receivedPointData.id}, lat $lat, long $long """)
        if (receivedPointData.id != "me") {
          geoData.replacePoint(lat, long, receivedPointData)
        }
        receivedPointData.id
      }
      else ""
    }
    val tryGraph = parseJsonLDAndSave(jsonString)
    println(s"DriversRideDB.makeJSONLDforAllPoints Try[Graph] $tryGraph")

    val receivedPointId = updateSenderPointIn2D_Database()
    val positionsMessage = makeJSONLDforAllPoints(jsonString, currentTimestamp)
    val deferredMessage = messageStack.popGraphMessage(receivedPointId)
    println( s"makeJSONforAllPoints: deferredMessage for <$receivedPointId> : $deferredMessage" )
    val result = messageStack.mergeGraphsAsJSONLDString(positionsMessage, deferredMessage)
    println(s"Sending result: sizes: positionsMessage ${getTriples(positionsMessage).size}, deferredMessage ${getTriples(deferredMessage).size}")
    if (result != "{}" ) recordMessage( serviceName + "-sent", result)
    Ok(result)
      .withHeaders("Access-Control-Allow-Origin" -> "*")
  }

  /** tolerate both a JS string or a JS number */
  def getFloat(js: JsLookupResult): Float = {
    js.get match {
      case s:JsString => s.as[String].toFloat
      case js => js.as[Float]
    }
}

}
