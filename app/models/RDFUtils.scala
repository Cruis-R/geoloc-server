package models

import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.Prefix
import org.w3.banana.RDFStore
import org.w3.banana.SparqlUpdate
import scala.util.Try
import org.w3.banana.SparqlOps
import org.w3.banana.SparqlEngine
import scala.util.Success
import scala.util.Failure
import org.w3.banana.PrefixBuilder

/** RDF OPerations on a DataBase
 *  COPIE' de semantic_forms */
trait RDFOPerationsDB[Rdf <: RDF, DATASET] {
    /** NOTE: same design pattern as for XXXModule in Banana */
  implicit val rdfStore: RDFStore[Rdf, Try, DATASET] with SparqlUpdate[Rdf, Try, DATASET]
  implicit val ops: RDFOps[Rdf]
  implicit val sparqlOps: SparqlOps[Rdf]
  implicit val sparqlGraph: SparqlEngine[Rdf, Try, Rdf#Graph]
}


trait RDFCommons[Rdf <: RDF] {
	implicit val ops: RDFOps[Rdf]
  import ops._

	val geo = new PrefixBuilder[Rdf]("geo", geoPrefix
//	    "http://www.w3.org/2003/01/geo/wgs84_pos#"
	    ) {
    val long = apply("long")
    val lat = apply("lat")
  }
	val geo_prefix = geo // Prefix[Rdf]("geo", geoPrefix)
  val driver_prefix = Prefix[Rdf]("driver", ontologyPrefix)
  
  /** get All RDF nodes having an rdf:type */
  def getAllInstances(graph: Rdf#Graph)(implicit ops: RDFOps[Rdf]) = {
    val typeTriples = find(graph, ANY, rdf.typ, ANY).toList
    typeTriples . map { t => t.subject } . toSet
  }
}

trait RDFUtils[Rdf <: RDF, DATASET] extends RDFOPerationsDB[Rdf, DATASET]
with RDFCommons[Rdf] {
  
	/**
   * this in-memory  store apparently does not support Transactions;
   *  NOTE: this should be done at Banana level
   */
  private val supportsTransactions = false
  
	import ops._
  import sparqlOps._
  import rdfStore.sparqlEngineSyntax._
  import rdfStore.transactorSyntax._
  import rdfStore.sparqlUpdateSyntax._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  /*  COPIE' de semantic_forms
   *  ======================== */

  /** run SPARQL on given dataset, knowing result variables; transactional */
  def sparqlSelectQueryVariables(queryString: String, variables: Seq[String],
                                 ds: DATASET): List[Seq[Rdf#Node]] = {
    if (supportsTransactions) {
      val transaction = ds.r({
        sparqlSelectQueryVariablesNT(queryString, variables, ds)
      })
      transaction.get
    } else
      sparqlSelectQueryVariablesNT(queryString, variables, ds)
  }

  /** run SPARQL on given dataset, knowing result variables; NOT transactional */
  def sparqlSelectQueryVariablesNT(queryString: String, variables: Seq[String],
                                   ds: DATASET): List[Seq[Rdf#Node]] = {
    val solutionsTry = for {
      query <- parseSelect(queryString)
      es <- ds.executeSelect(query, Map())
    } yield es
    //    println( "solutionsTry.isSuccess " + solutionsTry.isSuccess )
    val answers: Rdf#Solutions = solutionsTry.get
    val results = answers.iterator.toIterable map {
      row =>
        //        println( row )
        for (variable <- variables) yield {
          val cell = row(variable)
          cell match {
            case Success(node) => row(variable).get.as[Rdf#Node].get
            case Failure(f)    => Literal(".")
          }
        }
    }
    results.to[List]
  }
}