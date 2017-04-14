package models

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

//import _root_.models.ontologyPrefix
//import _root_.models.geoPrefix

import org.w3.banana.jena.JenaModule
import java.io.Reader

// models.JSONLDParser
object JSONLDParser extends App with JenaModule with JSONLDUtilsTrait[Jena] {
  println( parseJsonLDFile(args(0)) )
}

trait JSONLDUtilsTrait[Rdf <: RDF] {
  implicit val jsonldReader: RDFReader[Rdf, Try, JsonLd]

  /** parse given Json-LD File */
  def parseJsonLDFile(jsonLd: String, base: String=""): Try[Rdf#Graph] = {
    println(s"parseJsonLD( jsonLd $jsonLd") // >>>>>>>>>>>>>><
    parseJsonLDReader(new FileReader(jsonLd), base)
  }
  
  /** parse given Json-LD String */
  def parseJsonLDString(jsonLd: String, base: String=""): Try[Rdf#Graph] = {
    println(s"parseJsonLD( jsonLd $jsonLd") // >>>>>>>>>>>>>><
    parseJsonLDReader(new StringReader(jsonLd), base)
  }
  
  /** parse given Json-LD Reader String */
  def parseJsonLDReader(jsonLd: Reader, base: String=""): Try[Rdf#Graph] = {
    println(s"parseJsonLD( jsonLd $jsonLd") // >>>>>>>>>>>>>><
    val tryGraph = jsonldReader.read(jsonLd, base)
    tryGraph match {
      case Success(graph) =>
      case Failure(e) =>
        println(s"parseJsonLD: $e")
    }
    tryGraph
  }  
}
