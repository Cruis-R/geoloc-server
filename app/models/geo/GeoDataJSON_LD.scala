package models.geo

import java.util.Date

import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.syntax._
import org.w3.banana.binder._
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

import models.driver.DriversDB
import _root_.models.ontologyPrefix
import _root_.models.geoPrefix
import org.w3.banana.FOAFPrefix
import controllers.GeoDataDrivers
import org.w3.banana.RDFPrefix
import scala.util.Try
import scala.util.Success
import org.w3.banana.PointedGraphs
import models.RDFCommons

object GeoDataJSON_LD extends JenaModule with GeoDataJSON_LDTrait[Jena]

trait GeoDataJSON_LDTrait[Rdf <: RDF]
extends RDFLiteralConversions[Rdf]
with RDFCommons[Rdf] {
  implicit val ops: RDFOps[Rdf]
  import ops._
  val foaf = FOAFPrefix[Rdf]
  val rdf = RDFPrefix[Rdf]

  /**
   * @return message JSON-LD pour tous les utilisateurs géolocalisés;
   *  sauf ceux qui sont inactifs depuis plus de 40s
   */
  def makeJSONLDforAllPoints(jsonString: String, currentTimestamp: Date): Rdf#Graph = {
    val followedObjets = GeoDataDrivers.getFollowedObjets
    println(s"${currentTimestamp.toString()} getFollowedObjets " +
      followedObjets.mkString("[", " ; ", "]"))

    val list = for {
      obj <- followedObjets
      storedPointData = obj.data
      if currentlyActive(storedPointData, currentTimestamp)
      lat = obj.x
      long = obj.y
      name = DriversDB.findFOAFname(storedPointData.id).getOrElse(storedPointData.id)
    } yield {
      makeTriples(storedPointData.id, long, lat, name, storedPointData.timestamp)
    }
    val followedObjetsTriples = list.toSeq

    /* hack: add one triple to get same JSON-LD formatting
     * TODO move this elsewhere: if there is a deferred message, no need to do this
     */
    val followedObjetsTriples2 = if (followedObjets.size == 1)
      followedObjetsTriples.+:(Graph(Triple(URI("v"), rdf.typ, URI("Velo"))))
    else followedObjetsTriples

    println(s"makeJSONLDforAllPoints: points count ${followedObjetsTriples.size}")
    union(followedObjetsTriples2)
  }

  /**
   * checking that the mobile is actually sending positions,
   * by comparing the Timestamp in given PointData with current time:
   * @return if Timestamp is too old: false
   */
  def currentlyActive(storedPointData: PointData, currentTimestamp: Date): Boolean = {
    currentTimestamp.getTime - storedPointData.timestamp < 40000
  }

  /** make Triples for a driver:Driver object */
  def makeTriples(id: String, long: Float, lat: Float, name: String,
                  timestamp: Long) = (
    URI(id)
    -- URI(geoPrefix + "lat") ->- lat
    -- URI(geoPrefix + "long") ->- long
    -- foaf.name ->- name
    -- URI(ontologyPrefix + "timestamp") ->- timestamp
    -- rdf.typ ->- URI(ontologyPrefix + "Driver")).graph

  /** get Id by class From Graph
   * regarder ?X rdf:type <classId> .
   */
  def getIdFromGraphAndClass(graph: Rdf#Graph, classId: Rdf#URI): Option[Rdf#URI] = {
    val graphTriples = find(graph,
      ANY, rdf.typ, classId).toList
    //    println( s"getIdFromGraph: graphTriple $graphTriple" )
    graphTriples match {
      case triple :: rest =>
        val graphNode = triple.subject
        foldNode(graphNode)(
          graphURI => {
            println(s"getIdFromGraph: graphURI $graphURI")
            Some(graphURI)
          },
          _ => None, _ => None)
      case _ => None
    }
  }

  def getIdFromGraph(graph: Try[Rdf#Graph], classId: Rdf#URI): Option[Rdf#URI] = {
    val v = graph.map { gr => getIdFromGraphAndClass(gr, classId) }
    //        v match { case Success(x) => x ; case _ => None }
    v.toOption.flatten
  }

  /** get Coordinates from given graph
   *  @ return (long, lat) */
  def getCoordinates(dep: PointedGraphs[Rdf]): Option[(Float, Float)] = {
    val longOption = (dep / geo_prefix("long")).nodes.headOption
    val latOption = (dep / geo_prefix("lat")).nodes.headOption
    (longOption, latOption) match {
      case (Some(long), Some(lat)) => Some(
        long.as[Float].get,
        lat.as[Float].get)
      case _ => None
    }
  }
}
