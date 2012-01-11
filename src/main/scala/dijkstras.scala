package Graphrandy

import scala.collection.mutable.HashMap
import scala.Math


//class Node(var name: String) {
//	override def toString(): String = name
//}

class Graph(var nodes: HashMap[String,HashMap[String,Double]]) {
	def this() = {
		this(HashMap[String,HashMap[String,Double]]())	
	}
	
	// Add new node (does nothing if node already exists)
	def addNewNode(node: String): Unit = {
		nodes.getOrElseUpdate(node, HashMap[String,Double]())
	}
	
	// Adds new edge (creates new nodes if necessary)
	def addEdge(from: String, to: String, weight: Double) = {
		nodes.get(from) match {
			case None => {
				nodes += ((from, HashMap[String,Double]()))
				nodes(from) += ((to, weight))
			}
			case Some(x) => x += ((to, weight))			
		}

		nodes.getOrElseUpdate(to, HashMap[String,Double]())
	}
	
	def addEdge(t:(String,String,Double)): Unit = {
		this.addEdge(t._1, t._2, t._3)
	}
	
	// Deletes edge; does nothing if edge doesn't exist
	def delete(from: String, to: String) = {
		nodes.get(from) match {
			case None => {} // do nothing
			case Some(x) => x.remove(to)
		}
	}
	
	// Deletes node and all edges associated with it
	def delete(node: String) = {
		nodes.remove(node)
		for((k,v) <- nodes) {
			v.remove(node)
		}
	}
	
	// Returns edge weight (option)
	def getEdgeValue(from:String, to:String): Option[Double] = {
		nodes.get(from) match {
			case None => None
			case Some(x) => x.get(to)
		}
	}
	
	// Returns map of neighbors and the weights of that edge (option)
	def neighbors(from:String): Option[HashMap[String,Double]] = {
		nodes.get(from) match {
			case None => None
			case Some(x) => Option[HashMap[String,Double]](x)
		}
	}
	
	// Returns true if node or edge exists in the graph, false otherwise
	def contains(node:String): Boolean = nodes.contains(node)
	def contains(from:String, to:String): Boolean = {
		nodes.get(from) match {
			case None => false
			case Some(x) => x.contains(to)
		}
	}
}


object Dijkstra {
	def Dij (graph: Graph, source: String): HashMap[String,Double] = {
		// Initializations
		var dist = HashMap[String,Double]() 
		for (node <- graph.nodes.keys) {
			dist += ((node, Double.PositiveInfinity))
		}
			
		// Distance from source to source is 0
		dist.put(source, 0) 
		
		var Q = scala.collection.mutable.Set() ++ dist.keySet
		
		var break = false
		while(!Q.isEmpty && !break) {
			val temp = dist.filter((t) => Q.contains(t._1))
			val u = temp.minBy((t) => t._2)._1
			
			if (dist(u) == Double.PositiveInfinity) break = true
			else {
				Q -= u
				for (v <- graph.nodes(u)) {
					val alt = dist(u) + v._2
					if(alt < dist(v._1)) {
						dist(v._1) = alt				
					}
				}
			}
		}
		dist
	}
	
	def main(args: Array[String]) {
		var g = new Graph()

		
		// ex1
//		g.addEdge(("A","B",12))
//		g.addEdge(("A","C",14))
//		g.addEdge(("B","D",-3))
//		g.addEdge(("B","E",10))
//		g.addEdge(("C","D",6))
//		g.addEdge(("C","E",4))
//		g.addEdge(("D","E",5))
//		g.addEdge(("D","F",4))
//		g.addEdge(("E","F",-2))

		//ex2
		g.addEdge(("A","B",10))
		g.addEdge(("A","D",8))
		g.addEdge(("B","C",2))
		g.addEdge(("C","D",-4))
		g.addEdge(("D","E",-1))
		g.addEdge(("D","F",9))
		g.addEdge(("D","J",3))
		g.addEdge(("F","I",15))
		g.addEdge(("G","D",5))
		g.addEdge(("G","H",2))
		g.addEdge(("H","I",7))
		g.addEdge(("I","J",11))
		g.addEdge(("J","E",6))
		g.addEdge(("J","G",1))
		g.addEdge(("K","J",99))
		
		val dist = Dij(g,"A")
		println(dist)
	}
}
