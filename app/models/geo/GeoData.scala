package models.geo

import archery._
import scala.collection._

/**
 * in-memory data structure for geolocalized objects followed by the application;
 * uses Archery, efficient sorted data structure for 2D geographical data
 * See https://github.com/meetup/archery
 * 
 * NOTE: n'exporte pas de dépendence à Archery
 */
trait GeoDataTrait {

  private val followedObjetsMap = mutable.Map[String, Entry[PointData]]()
  private var followedObjets: RTree[PointData] = RTree()

  /** replace Point
   *  @param lat, long coordinates (in degree)
   *  @param pointData : contains id */
  def replacePoint(lat: Float, long: Float, pointData: PointData) = {
    // TODO éviter ce goulot d'étranglement, peut être STM "software transactional memory", ou AtomicReference
    this.synchronized {
        val newPoint = Entry(Point(lat, long), pointData)
        val oldPoint = followedObjetsMap.getOrElse(pointData.id, newPoint)
        if (!followedObjetsMap.contains(pointData.id))
          println(s"New followed Objet '${pointData.id}'")
        followedObjetsMap.update(pointData.id, newPoint)
        followedObjets = followedObjets.remove(oldPoint)
        followedObjets = followedObjets.insert(newPoint)
      }
  }

  val convertToSpacePointId = (entry: Entry[PointData] ) =>
    new SpacePointData( entry.geom.x, entry.geom.y, entry.value )

  /** @return renvoie la liste des objets géolocalisés
   *  n'exporte pas de dépendence à Archery */
  def getFollowedObjets(): Iterable[SpacePointData] = {
    val list = followedObjetsMap.values
    list . map { convertToSpacePointId }
  }
    
  /** @return renvoie la liste des k objets les plus proches
   *  @param lat, long coordinates (in degree)
   *  @param k number of neighbors wanted */
  def nearestK(lat: Float, long: Float, k: Int): IndexedSeq[SpacePointData] = {
    val list = followedObjets.nearestK( Point(lat, long), k)
    list . map { convertToSpacePointId }
  }

}