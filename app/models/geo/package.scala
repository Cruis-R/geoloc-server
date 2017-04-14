import org.w3.banana.binder.FromLiteral
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import org.w3.banana.FailedConversion


package models {
  package object geo {
    import org.w3.banana.RDF
    import org.w3.banana.RDFOps
    import org.w3.banana.binder.ToLiteral

    /** Data associated to a Point, currently URI of a user person, and Unix timestamp of last sign of life */
    case class PointData(id: String, timestamp: Long)

    type SpaceCoord = Float
    type TimeStamp = Long

    /* TODO mettre dans Banana dans trait ToLiteral.scala	*/
    trait RDFLiteralConversions[Rdf <: RDF] {
      implicit def LongToLiteral[Rdf <: RDF](implicit ops: RDFOps[Rdf]) =
        new ToLiteral[Rdf, Long] {
          import ops._
          def toLiteral(i: Long): Rdf#Literal = Literal(i.toString, xsd.long)
        }
      implicit def FloatToLiteral[Rdf <: RDF](implicit ops: RDFOps[Rdf]) =
        new ToLiteral[Rdf, Float] {
          import ops._
          def toLiteral(i: Float): Rdf#Literal = Literal(i.toString, xsd.float)
        }
      implicit def DoubleToLiteral[Rdf <: RDF](implicit ops: RDFOps[Rdf]) =
        new ToLiteral[Rdf, Double] {
          import ops._
          def toLiteral(i: Double): Rdf#Literal = Literal(i.toString, xsd.float)
        }
    }
    
    /* TODO mettre dans Banana dans trait FromLiteralscala */
    implicit def FloatFromLiteral[Rdf <: RDF](implicit ops: RDFOps[Rdf]) =
      new FromLiteral[Rdf, Float] {
    import ops._
    def fromLiteral(literal: Rdf#Literal): Try[Float] = {
      val Literal(lexicalForm, datatype, _) = literal
      if (datatype == xsd.float) {
        try {
          Success(lexicalForm.toFloat)
        } catch {
          case _: NumberFormatException =>
            Failure(FailedConversion(
                s"${literal} is an xsd.double but is not an acceptable double value."))
        }
      } else {
        Failure(FailedConversion(s"${literal} is not an xsd:float"))
      }
    }
  }
  }
}