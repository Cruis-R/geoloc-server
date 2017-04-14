package models.geo

case class SpacePoint(val x: SpaceCoord, val y: SpaceCoord) {
	def long = x
  def lat = y
  
  def this( pair: (SpaceCoord, SpaceCoord) ) =  this( pair._1, pair._2 )
}

/** Point + data */
class SpacePointData(x: SpaceCoord, y: SpaceCoord, val data: PointData)
    extends SpacePoint(x, y) {
  override def toString() = s"id: ${data.id}, long: $x, lat: $y"
}

/** coordonnées spatio-temporelles */
class SpaceTimePoint(x: SpaceCoord, y: SpaceCoord, val time: TimeStamp)
  extends SpacePoint(x, y) {}

/** trajet: 2 coordonnées spatio-temporelles */
case class Path(departure: SpaceTimePoint, arrival: SpaceTimePoint) {}
