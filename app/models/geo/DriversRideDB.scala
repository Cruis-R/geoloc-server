package models.geo

import java.io.FileReader
import java.net.{ URI => jURI }
import java.security.MessageDigest

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.w3.banana.FOAFPrefix
import org.w3.banana.PointedGraph
import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.RDFPrefix
import org.w3.banana.io.RDFReader
import org.w3.banana.io.Turtle
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

import javax.xml.bind.annotation.adapters.HexBinaryAdapter
import java.net.URLEncoder
import org.w3.banana.io.JsonLd
import org.w3.banana.io.RDFWriter
import org.w3.banana.io.JsonLdCompacted
import java.io.StringReader
import org.apache.jena.graph.Factory

import org.w3.banana.PrefixBuilder
import org.w3.banana.MGraphOps
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.query.DatasetFactory
import org.w3.banana.jena.JenaDatasetStore
import org.w3.banana.RDFStore
import org.w3.banana.SparqlUpdate
import models.driver.DriversDBTrait

import _root_.models.ontologyPrefix
import _root_.models.geoPrefix
import models.RDFUtils
import models.ApplicationSettings
import org.apache.jena.query.{Dataset => DATASET_TYPE}
import org.w3.banana.PointedGraphs

object DriversRideDB
    extends ApplicationSettings
    with RideDBTrait[Jena, DATASET_TYPE]
    with DriversDBTrait[Jena]
    with RDFUtils[Jena, DATASET_TYPE] {

  // initialize in-memory RDF storage

  override val dataset = DatasetFactory.createMem()
  val model = ModelFactory.createDefaultModel()
  val graphURI = "urn:geo"
  dataset.addNamedModel(graphURI, model)
  val graphNode = model.createResource(graphURI).asNode()
  override val graphDatabase = dataset.asDatasetGraph().getGraph(graphNode)
  override val rdfStore = new JenaDatasetStore(false)
//  import rdfStore.graphStoreSyntax._
}

/**
 * base de données dynamique en mémoire sur les courses et les chauffeurs;
 *  TODO : charger aussi DriversDB (chauffeurs + mots de passe)
 *  NOTE: on a 2 bases de données en mémoire:
 *  - celle-ci, un graphe RDF,
 *  - la base données 2D (R-tree) GeoData, optimisée pour recherche de proximité
 *
 * (voir doc/conception.md)
 */
trait RideDBTrait[Rdf <: RDF, DATASET]
    extends RDFUtils[Rdf, DATASET]
    		with GeoDataJSON_LDTrait[Rdf] {
  
  implicit val ops: RDFOps[Rdf]
  import ops._

  val dataset: DATASET

  implicit val jsonldReader: RDFReader[Rdf, Try, JsonLd]

  // no dependency to semantic_forms !
  val graphDatabase: Rdf#MGraph

  /** parse given Json-LD */
  def parseJsonLD(jsonLd: String): Try[Rdf#Graph] = {
    println(s"parseJsonLD( jsonLd $jsonLd") // >>>>>>>>>>>>>><
    val tryGraph = jsonldReader.read(new StringReader(jsonLd), "")
    tryGraph match {
      case Success(graph) =>
      case Failure(e) =>
        println(s"parseJsonLDAndSave: replace possible triple: $e")
    }
    tryGraph
  }

  def getExistingTriples(incomingID: Rdf#Node,
      graph: Rdf#Graph = graphDatabase.makeIGraph() ): List[Rdf#Triple] =
    find(graph, incomingID, ANY, ANY).toList

  def getExistingTriplesAsGraph(incomingID: Rdf#Node): Rdf#Graph = {
    ops.makeGraph(
      getExistingTriples(incomingID))
  }
  
  private def removeTriplesHavingSubject(incomingID: Rdf#Node) = {
    val existingTriples = getExistingTriples(incomingID)
    println(s"\tremoveTriplesHavingSubject( existingTriples $existingTriples )")
    removeTriples(graphDatabase, existingTriples)
  }

  /**
   * parse given Json-LD for given instances ID and class ID,
   *  and Save in graph Database,
   *  replacing existing Triples for these instance ID's with given JSON-LD;
   *  this includes driver position,
   *  and ride information
   */
  def parseJsonLDAndSave(jsonLd: String): Try[Rdf#Graph] = {
    val incomingGraphTry = parseJsonLD(jsonLd)
    replaceExistingTriples(incomingGraphTry)
    incomingGraphTry
  }

  /** Save in graphDatabase & replace existing Triples for each instance ID */
  def replaceExistingTriples(incomingGraphTry: Try[Rdf#Graph]) = {
        val allInstances = getAllInstances(incomingGraphTry.get)
    for (instance <- allInstances) {
      removeTriplesHavingSubject(instance)
    }
    addTriples(graphDatabase, incomingGraphTry.get.triples)
  }
  
  /**
   * regarder dans le champ "departure" : lat, long
   */
  def getCoordinatesFromRideInquiry(rideInquiry: Rdf#Graph): Option[(Float, Float)] = {
    //    println( s"""getCoordinatesFromRideInquiry: rideInquiry $rideInquiry \n\t ${URI(ontologyPrefix + "RideEnquiry")}""" )
    val rideInquiryTriple = find(rideInquiry,
      ANY, rdf.typ, URI(ontologyPrefix + "RideEnquiry")).toList
    //    println( s"getCoordinatesFromRideInquiry: rideInquiryTriple $rideInquiryTriple" )

    rideInquiryTriple match {
      case triple :: rest =>
        val rideInquiryNode = triple.subject
        foldNode(rideInquiryNode)(
          rideInquiryURI => {
            println(s"getCoordinatesFromRideInquiry: rideInquiryURI $rideInquiryURI")
            val dep = PointedGraph(rideInquiryURI, rideInquiry) /
              URI(ontologyPrefix + "departure")
            getCoordinates(dep)
          },
          _ => None, _ => None)
      case _ => None
    }
  }

  /**
   * find current ride if any: is there a ride whose "from" or "passengerID"
   * matches receivedPassengerId ?
   */
  def findcurrentRideFromPassenger(
    tryIncomingGraph: Try[Rdf#Graph],
    receivedPassengerId: String): List[Rdf#Node] = {
    val rideTriples = ops.find(
      graphDatabase.makeIGraph(), ANY,
      driver_prefix("passengerID"), URI(receivedPassengerId)).toList
      rideTriples . map { tr => tr.subject }
//    // Get all triples <ride> ?P ?V . ?V ?P1 ?V1 .
//    getTreeLenght2(rides)
  }

  /**
   * find current ride if any: is there a ride whose "serviceStartedBy"
   * matches receivedDriverId ?
   * @return the current ride URI as a List of nodes (normally 0 or one)
   */
  def findcurrentRideFromDriver(
    tryIncomingGraph: Try[Rdf#Graph],
    receivedDriverId: String):
    List[Rdf#Node] = {
    println( s"findcurrentRideFromDriver: graphDatabase $graphSummary" )
    val rideTriples = ops.find(
      graphDatabase.makeIGraph(), ANY,
      driver_prefix("serviceStartedBy"),
      URI(receivedDriverId)).toList
    println( s"findcurrentRideFromDriver: receivedDriverId <$receivedDriverId> : rideTriples $rideTriples" )
    rideTriples . map { tr => tr.subject }
  }

  def graphSummary() = {
    val insts = getAllInstances( graphDatabase.makeIGraph() )
    graphDatabase.size + " : " + insts. mkString(", ")
  }
  
  /** @return URI of the Other Person (driver from passenger and vice versa) */
  def getOtherPersonID(currentRides: List[Rdf#Node], mobilePersonId: String ): Option[Rdf#Node] = {
    val v = for (
      currentRide <- currentRides;
      graph = graphDatabase.makeIGraph();
      pg = PointedGraph(currentRide, graph);
      serviceStartedByTriples = ops.find(graph, ANY, driver_prefix("serviceStartedBy"), ANY);
      serviceStartedByTriple <- serviceStartedByTriples;
      passengerIDTriples = ops.find(graph, ANY, driver_prefix("passengerID"), ANY);
      passengerIDTriple <- passengerIDTriples
    ) yield {
      val passenger = passengerIDTriple.objectt
      val driver = serviceStartedByTriple.objectt
      println(s"passenger $passenger, driver $driver")
      if (mobilePersonId == passenger)
        driver
      else
        passenger
    }
    v.headOption
  }

  /** @return URI of the Other Person (driver from passenger and vice versa) */
  def getOtherPersonIDFromID(currentRide: String, mobilePersonId: String ): Option[Rdf#Node] =
	  getOtherPersonID(List(URI(currentRide)), mobilePersonId)

  def getOtherPersonGraph(rideId: String, deferredRecipientID: String): Rdf#Graph = {
    val otherPersonIDOption = getOtherPersonIDFromID(rideId, deferredRecipientID)
    val otherPersonGraph = for (
      otherPersonID <- otherPersonIDOption
    ) yield {
      getExistingTriplesAsGraph(otherPersonID)
    }
    otherPersonGraph.getOrElse(emptyGraph)
  }
  
  def union( g1: Rdf#Graph, g2: Rdf#Graph ): Rdf#Graph = g1 union g2

  /** get passenger graph From Ride Graph */
  def getpassengerFromRideGraph(rideGraph: Rdf#Graph, rideId: String): Rdf#Graph = {
			val graph = graphDatabase.makeIGraph() ;
    val passengerIDTriples = ops.find(rideGraph, URI(rideId), driver_prefix("passengerID"), ANY)
	  val v = for (
			        passengerIDTriple <- passengerIDTriples ;
			        passengerTriples = ops.find( graph, passengerIDTriple.objectt, ANY, ANY )
			  ) yield { passengerTriples }
    ops.makeGraph( v.flatten.toIterable)
  }

  /**
   * Get get Tree of Lenght 2, that is get all triples <ride> ?P ?V . ?V ?P1 ?V1 .
   * UNUSED TODO move it elsewhere
   */
  private def getTreeLenght2(rides: List[Rdf#Triple]): List[Rdf#Triple] = {
    // Get all triples <ride> ?P ?V . ?V ?P1 ?V1 .
    val w = rides.map { tr =>
      val ride = tr.objectt
      val spos = find(graphDatabase.makeIGraph(), ride, ANY, ANY).toList
      val v = for (spo <- spos) yield {
        find(graphDatabase.makeIGraph(), spo.objectt, ANY, ANY).toList
      }
      val vv = v.flatten
      vv
    }
    w.flatten
  }
}
