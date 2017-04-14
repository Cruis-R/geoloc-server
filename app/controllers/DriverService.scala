package controllers

import models.geo.GeoDataTrait
import org.w3.banana.jena.Jena
import org.apache.jena.query.{Dataset => DATASET}
import models.geo.DriversRideDB
import org.w3.banana.jena.JenaModule
import models.MessageStack
import models.geo.SpacePoint
import scala.util.Try
import org.w3.banana.RDF
import models.geo.RideDBTrait
import models.MessageStackRDF

object GeoDataDrivers extends GeoDataTrait

/** service /position : renvoie tous les chauffeurs actifs,
 *  plus un éventuel message différé */
object DriverService extends JenaModule with PositionService[Jena, DATASET] {
  
  val geoData = GeoDataDrivers
  override val graphDatabase = DriversRideDB.graphDatabase
  override val dataset = DriversRideDB.dataset
  override val messageStack = MessageStack
}

trait DriverServiceTrait[Rdf <: RDF, DATASET] extends PositionService[Rdf, DATASET]
with RideDBTrait[Rdf, DATASET] {
}
