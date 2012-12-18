---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search Algorithms
	
	Script		: BreadthFirstSearch.hs - This module has
				  implementation of Breadth First Search
				  Algorithm
-}
---------------------------------------------------------
module BreadthFirstSearch
	( bfs,				-- [Edge] -> Int -> [Int]
	  findEdge,			-- Int -> Node -> [Edge] -> Queue Node -> Queue Node -> Queue Node
	  exploreNewVertex,	-- Int -> Edge -> [Edge] -> Queue Node -> Queue Node -> Queue Node
	  showPath			-- Queue Node -> [Int]
	) where

import Graphs
import Queue
import Vertex

{-
	This fucntion gets a graph and a goal node, and gives
	a list of nodes to get to that goal node. If a node is 
	not found, an empty list is produced. 
-}
bfs 								:: [Edge] -> Int -> [Int]
bfs [] _							= []
bfs g goal
	| get (fst1(head g)) == goal	= [goal]
	| otherwise						= reverse (showPath 
										(findEdge goal (fst1(head g)) g 
											(add (fst1(head g)) newQueue) 
											 (add (fst1(head g)) newQueue)))

-- Function: findEdge
{-
	This fucntion find an adjacent edge in order to explore
	the adjacent vertex.
-}
findEdge 								:: Int -> Node -> 
											[Edge] -> 
											Queue Node -> 
											Queue Node ->
											Queue Node
findEdge goal v1 g oq cq
	| isEmpty oq						= newQueue	-- An empty Queue
	| isEdgeExist v1 g	(showPath cq)	= exploreNewVertex 
											goal (getEdge v1 g (showPath cq)) g oq cq
	| otherwise							= findEdge goal 
											(top (snd(remove oq))) 
											g 
											(snd(remove oq))
											cq

-- Function: exploreNewVertex
{-
	This function examines a particular vertex whether it is 
	the goal node.
-}
exploreNewVertex							:: Int -> Edge -> [Edge] -> 
												Queue Node -> 
												Queue Node -> 
												Queue Node
exploreNewVertex goal e g oq cq
	| goal == (get(snd1 e))					= add (snd1 e) cq
	| otherwise								= findEdge goal (top oq) 
												(visitGraph e g) 
												(add (snd1 e) oq) 
												(add (snd1 e) cq)

-- Function: showPath
{-
	This fucntion gets all values from the Queue
-}
showPath			:: Queue Node -> [Int]
showPath oq			= [get x | x <- (printData oq)]

-- END
