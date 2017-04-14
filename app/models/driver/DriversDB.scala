package models.driver

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
import _root_.models.usersOntologyPrefix

object DriversDB extends JenaModule with DriversDBTrait[Jena]


/** base de données en mémoire sur les chauffeurs (données "permanentes", pas les courses) */
trait DriversDBTrait[Rdf <: RDF] {
  implicit val ops: RDFOps[Rdf]
  implicit val turtleReader: RDFReader[Rdf, Try, Turtle]
//  implicit val turtleWriter: RDFWriter[Rdf, Try, Turtle]
  // implicit val jsonldReader: RDFReader[Rdf, Try, JsonLd]
  // implicit val rdfXMLReader: RDFReader[Rdf, Try, RDFXML]
  import ops._
  val passwordMD5Digest = URI( usersOntologyPrefix + "passwordMD5Digest")
  val passwordPred = URI( usersOntologyPrefix + "password")

  val dataURI = "chauffeurs.ttl"
  
  private val foaf = FOAFPrefix[Rdf]
  private val rdf = RDFPrefix[Rdf]
  
  /** read RDF data (users with passwords) from configured URI into graph */
  def readData() = {
    val triedGraph = Try {
      val reader = new FileReader(dataURI)
      turtleReader.read( reader, "" ) . get
    }
    triedGraph match {
      case Success(g) => g
      case Failure(e) =>
        println( s"readData $dataURI : $e" )
        ops.emptyGraph
    }
  }

  lazy val graph: Rdf#Graph = addPassWordDigest( readData() )

  /** compare password with database; @return user URI if success */
  def checkPassword(userUri: String, password:String): Boolean = {
    val databasePasswordOption = findPasswordDigest(userUri)
    databasePasswordOption match {
      case Some(databasePasswordDigest) =>
        println(s"checkPassword: databasePassword\t$databasePasswordDigest")
        println(s"checkPassword: hashPassword\t${hashPassword(password)}")
        val pwdMatch = databasePasswordDigest == hashPassword(password)
        println(s"checkPassword: pwd Match ${pwdMatch}")        
        pwdMatch
      case None =>
        println(s"checkPassword: Password Digest not found in database for user Uri '$userUri', password '$password'")
        false
    }
  }
  
  private def findPasswordDigest(userUri: String): Option[String] = {
    findAnyProperty(makeURIFromEMail(userUri), passwordMD5Digest)
  }

  def findFOAFname(userUri: String): Option[String] = {
    findAnyProperty(makeURIFromEMail(userUri), foaf.name)
  }
    
  private def findAnyProperty(userUri: String, prop: Rdf#URI): Option[String] = {
    val res = find(graph, URI(userUri), prop, ANY )
    if ( ! res.isEmpty ) {
      val obj = res.next().objectt
      foldNode(obj)( x=>None, x=>None,
          literal => Some(fromLiteral(literal)._1 ))
    } else None
  }
   
  private def hashPassword(password: String): String = {
    new HexBinaryAdapter().marshal(MessageDigest.getInstance("MD5").
        digest(password.getBytes))
  }

  def addPassWordDigest(data: Rdf#Graph): Rdf#Graph = {
    val persons = find(data, ANY, rdf.typ, foaf.Person )
    val passwordTriples = for {
      personTriple <- persons
      pg = PointedGraph(personTriple.subject, data)
      v = pg / passwordPred
      passwd <- v.nodes
    } yield {
      val password = foldNode(passwd)( _=>"",  _=>"", lit=>fromLiteral(lit)._1 )
      Triple( personTriple.subject, passwordMD5Digest,
        Literal(hashPassword(password) ) )
    }
    data union Graph( passwordTriples.toIterable )
  }
  
  val randomSeed = scala.util.Random
  def generatePassWordsForPersons(data: Rdf#Graph): Rdf#Graph = {
    val persons = find(data, ANY, rdf.typ, foaf.Person )
    val passwordTriples = for {
      personTriple <- persons
    } yield {
      val password = randomSeed.nextLong().toString()
      Triple( personTriple.subject, passwordPred, Literal(password) )
    }
    data union Graph( passwordTriples.toIterable )
  }

  def makeURIFromEMail(userUri: String): String = {
	  val encodedURI = URLEncoder.encode(userUri, "utf-8")
    val uri = new jURI(encodedURI)
    if( uri.isAbsolute() )
      userUri
    else if(uri.getScheme == "mailto" )
      userUri
    else
      "mailto:" + encodedURI
  }

}
