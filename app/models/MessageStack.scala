package models

import scala.collection.immutable.Stack
import org.w3.banana.jena.JenaModule
import org.w3.banana.jena.Jena
import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.io.RDFWriter
import org.w3.banana.io.JsonLdCompacted
import scala.util.Try

object MessageStack extends MessageStackTrait with ApplicationSettings
with MessageStackRDF[Jena] {
  /** Graph type */
  override type CONTENT = GRAPH // Jena#Graph
}

trait MessageStackRDF[Rdf <: RDF] extends MessageStackTrait {
  type CONTENT = Rdf#Graph
  implicit val ops: RDFOps[Rdf]
  import ops._
  implicit val jsonldCompactedWriter: RDFWriter[Rdf, Try, JsonLdCompacted]

  def mergeGraphs(g1: Rdf#Graph, g2: Rdf#Graph) = {
    g1. union( g2 )(ops)
  }

  /** récupérer message RDF indicé par l'URI (id) du receveur */
  def popGraphMessage(id: String): Rdf#Graph = {
    val option = super.popMessage(id)
    option match {
      case Some(g) => g
      case None => emptyGraph
    }
  }
  
  /** fusionner les 2 messages
   *  @return chaîne JSON-LD */
  def mergeGraphsAsJSONLDString(g1: Rdf#Graph, g2: Rdf#Graph): String = {
    val graph = mergeGraphs(g1, g2)  
    val mergedGraphs = jsonldCompactedWriter.asString(graph, "")
//    println(s"mergedGraphs: $mergedGraphs")
    mergedGraphs . getOrElse( """{ "error": "mergeGraphsAsJSONLDString $mergedGraphs"}""" )
  }
}

/** piles de Messages pour chaque receveur de messages */
trait MessageStackTrait {
  type CONTENT
  /* NOTE : le commentaire de deprécation de Stack me parait faux dans le détail:
   * list.tail renvoie l'extrémité droite, et elem :: list ajoute à gauche !!!  */
  private var messageStack = Map[String, Stack[CONTENT]]()

  /** stocker le message pour envoi ultérieur */
  def pushMessage(id: String, content: CONTENT): Unit = {
    messageStack = messageStack + (id -> {
      val stack = messageStack.getOrElse(id, Stack())
      stack.push(content)
    })
  }

  def popMessage(id: String): Option[CONTENT] = {
    println(s"popMessage $id messageStack $messageStack")
    val stack = messageStack.getOrElse(id, Stack())
    val ret = stack.headOption
    val newStack = if( ! stack.isEmpty ) stack.pop else stack
    messageStack = messageStack + ( id -> newStack )
    ret
  }

}