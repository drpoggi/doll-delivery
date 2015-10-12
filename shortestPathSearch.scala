/**
 * Scala (ver. 2.11.7) implementation of Dijkstra's Algorithm for https://github.com/postnati/doll-delivery
 * Created by David Poggi
 */

import scala.collection.mutable.ListBuffer
import scala.collection.{Set, mutable}

class Vertex(name:String) {
  val connectedTo = mutable.Map.empty[Vertex, Int] //Adjancey list, each vertex stores the vertices it's connected to and the distance of the connection

  def addConnection(neighbor:Vertex, distance: Int): Unit ={
    connectedTo += (neighbor -> distance)
  }

  def getDistance(neighbor:Vertex): Int = {
     connectedTo.get(neighbor).get
  }

  def getConnections: Set[Vertex] = {
    connectedTo.keySet
  }

  override def toString: String = {
    name
  }
}

class Graph {
  val vertexList = mutable.Map.empty[String, Vertex]

  def addVertex(name:String): Unit = {
      val newVertex = new Vertex(name)
      vertexList += (name -> newVertex)

  }

  def addEdge(from:String , to:String, distance:Int = 0): Unit = {
    if (!vertexList.contains(from))
      addVertex(from)
    if (!vertexList.contains(to))
      addVertex(to)
    vertexList(from).addConnection(vertexList(to), distance)
  }
}

object graphFunctions {
  def buildGraph(edges: List[Map[String, Any]]): Graph = {
    val g = new Graph()
    for (location <- edges) {
      /*Had to use asInstanceOf to preserve types of the rest of the code, or else we'll have Anys everywhere, since the source edge Map is String -> Any*/
        g.addEdge(location("startLocation").asInstanceOf[String], location("endLocation").asInstanceOf[String], location("distance").asInstanceOf[Int])
    }
    g
  }


  def dijkstra(graph: Graph, startingLocation: Vertex, targetLocation:Vertex): Any = {
    val distance = mutable.Map[Vertex, Int]() += (startingLocation -> 0) //shortest distance from vertex to source
    val previous = mutable.Map[Vertex, Any]() += (startingLocation -> None) //Use None to represent that no previous vertex exists (initially)

    val unvisitedVertices = mutable.Set.empty[Vertex]

    for (vertex <- graph.vertexList.values) {
      // Initialization code that sets every vertex other than the source as infinite (not really but close enough)
      // distance from the source and with no previous vertex in the path
      if (vertex != startingLocation) {
        distance(vertex) = Int.MaxValue
        previous(vertex) = None
      }
      unvisitedVertices += vertex
    }

    while (unvisitedVertices.nonEmpty){
      var currentVert = unvisitedVertices.minBy(distance(_)) //finds the vertex with the least distance from the source vertex
      unvisitedVertices -= currentVert

      if (currentVert == targetLocation){
        var pathSequence = new ListBuffer[Vertex]()
        var source = targetLocation
        while (previous.contains(source)){
          pathSequence += source
          try {
            source = previous(source).asInstanceOf[Vertex]
          } catch {
            case ex: ClassCastException =>
              /*Exception occurs when source is None, which means we're back at the starting
              vertex (as it has no previous vertex) and can return the path and distance*/
              return (pathSequence.reverse, distance(targetLocation))
          }
        }
      }

      for(neighbor <- currentVert.getConnections) {
        // Looks at all the neighbors of our current vertex, and if the path from the neighbor to the source is shorter
        // through the current vertex than we update the neighbor's distance to the source with this new value and set the
        // previous vertex for the neighbor as our current vertex
        var altPath = distance(currentVert) + currentVert.getDistance(neighbor)
        if (altPath < distance(neighbor)) {
          distance(neighbor) = altPath
          previous(neighbor) = currentVert
        }
      }
    }

  }
}

object shortestPathSearch {
  def findShortestPath(startingLocation: String, targetLocation:String, edges: List[Map[String, Any]]): Map[String, Any] = {
    val g = graphFunctions.buildGraph(edges)
    val (path:mutable.ListBuffer[Vertex], distance:Int) = graphFunctions.dijkstra(g, g.vertexList(startingLocation), g.vertexList(targetLocation))

    Map("distance" -> distance,"path" -> path.mkString(" => "))
  }

  def main(args: Array[String]) {
    val edges = List(
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Mark's crib", "distance" -> 9),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Greg's casa", "distance" -> 4),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Matt's pad", "distance" -> 18),
      Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Brian's apartment", "distance" -> 8),
      Map("startLocation" -> "Brian's apartment", "endLocation" -> "Wesley's condo", "distance" -> 7),
      Map("startLocation" -> "Brian's apartment", "endLocation" -> "Cam's dwelling", "distance" -> 17),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Cam's dwelling", "distance" -> 13),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Mike's digs", "distance" -> 19),
      Map("startLocation" -> "Greg's casa", "endLocation" -> "Matt's pad", "distance" -> 14),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Kirk's farm", "distance" -> 10),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Nathan's flat", "distance" -> 11),
      Map("startLocation" -> "Wesley's condo", "endLocation" -> "Bryce's den", "distance" -> 6),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Mark's crib", "distance" -> 19),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Nathan's flat", "distance" -> 15),
      Map("startLocation" -> "Matt's pad", "endLocation" -> "Craig's haunt", "distance" -> 14),
      Map("startLocation" -> "Mark's crib", "endLocation" -> "Kirk's farm", "distance" -> 9),
      Map("startLocation" -> "Mark's crib", "endLocation" -> "Nathan's flat", "distance" -> 12),
      Map("startLocation" -> "Bryce's den", "endLocation" -> "Craig's haunt", "distance" -> 10),
      Map("startLocation" -> "Bryce's den", "endLocation" -> "Mike's digs", "distance" -> 9),
      Map("startLocation" -> "Mike's digs", "endLocation" -> "Cam's dwelling", "distance" -> 20),
      Map("startLocation" -> "Mike's digs", "endLocation" -> "Nathan's flat", "distance" -> 12),
      Map("startLocation" -> "Cam's dwelling", "endLocation" -> "Craig's haunt", "distance" -> 18),
      Map("startLocation" -> "Nathan's flat", "endLocation" -> "Kirk's farm", "distance" -> 3)
    )
    val startingLocaion = "Kruthika's abode"
    val targetLocation = "Craig's haunt"

    println(findShortestPath(startingLocaion, targetLocation, edges))
  }
}
