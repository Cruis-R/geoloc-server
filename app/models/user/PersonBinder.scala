package models.user

import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.binder.RecordBinder
import org.w3.banana.binder.PGBinder
import org.w3.banana.binder._
import org.w3.banana._, diesel._
import org.w3.banana.FOAFPrefix
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

import scala.util._

import _root_.models.schema_org
import _root_.models.ontologyPrefix

object PersonBinder extends JenaModule with PersonBinder[Jena]

trait PersonBinder[Rdf <: RDF] {
  
  implicit val ops: RDFOps[Rdf]
  implicit val recordBinder: RecordBinder[Rdf]
  import ops._
  import recordBinder._

  val foaf = FOAFPrefix[Rdf]
  val schema = Prefix[Rdf]( "schema", schema_org )
  
  val classe = foaf.Person
  implicit val classUris = classUrisFor[Person](classe)

  val firstName = property[String](foaf("firstName"))
  val lastName = property[String](foaf("lastName"))
  val mbox = property[EMail](foaf("mbox"))
  val telephone = property[TelephoneNumber](schema("telephone"))
  val adress = property[Adress](foaf("adress")) // TODO adress
  
  val binder0 = pgbWithId[Person](t => URI("mailto:" + t.email))
  implicit val binder: PGBinder[Rdf, Person] =
  binder0.apply(firstName, lastName, mbox, telephone, adress)(Person.apply, Person.unapply) withClasses classUris
}

private trait PersonBinderExample[Rdf <: RDF] extends PersonBinder[Rdf] {
    import ops._
  // exemple d'usage du mapping Sémantique --> Objet
  val gr: Rdf#Graph = emptyGraph // mettre un vrai graphe
  val pg = PointedGraph( URI("rr:rr"), gr )
  val pers: Try[Person] = pg.as[Person]

  // exemple d'usage du mapping Objet --> Sémantique : TODO <<<<<<<<<<<

}
