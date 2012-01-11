package Graphrandy

import scala.collection.mutable.HashMap
import org.specs2._
//import org.specs2.specification._
import Graphrandy._


class spec extends Specification { def is =
	"Specification for testing Dijkstra's algorithm"		^
															p^
	"A small shortest-path map should"						^ 
		"have distance 0 to origin"							! small().testOrigin ^
		"have same size as original graph"					! small().testSize ^	
		"have correct shortest-distance values"				! small().testValues ^
															endp^
	"A big shortest-path map should"						^ 
		"have distance 0 to origin"							! big().testOrigin ^
		"have same size as original graph"					! big().testSize ^	
		"have correct shortest-distance values"				! big().testValues ^
															end
			
	case class small() {
		var g = new Graph()
		g.addEdge(("A","B",12))
		g.addEdge(("A","C",14))
		g.addEdge(("B","D",-3))
		g.addEdge(("B","E",10))
		g.addEdge(("C","D",4))
		g.addEdge(("C","E",6))
		g.addEdge(("D","E",5))
		g.addEdge(("D","F",4))
		g.addEdge(("E","F",-2))
		val dist = Dijkstra.Dij(g, "A")
		
		def testOrigin() = {
			dist("A") must_== 0
		}
		
		def testSize() = {
			dist.size must_== g.nodes.size
		} 
		
		def testValues() = {
			dist must havePairs("B"->12, "C"->14, "D"->9, "E"->14, "F"->12)
		}
	}
															
	case class big() {
		var g = new Graph()
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
		val dist = Dijkstra.Dij(g, "A")
																
		def testOrigin() = {
			dist("A") must_== 0
		}
		
		def testSize() = {
			dist.size must_== g.nodes.size
		} 
		
		def testValues() = {
			dist must havePairs("B"->10, "C"->12, "D"->8, "E"->7, "F"->17, "G"->12, "H"->14, "I"->21, "J"->11, "K"->Double.PositiveInfinity)
		}
	}
	

}