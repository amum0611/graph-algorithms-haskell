---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search Algorithms
	
	Script		: BreadthFirstTraversal.hs - This module has
				  implementation of Breadth First Search
				  Algorithm
-}
---------------------------------------------------------
module BreadthFirstTraversal
	( bft,				-- [Edge] -> Int -> [Int]
	  findEdge,			-- Int -> Node -> [Edge] -> Queue Node -> Queue Node -> Queue Node
	  exploreNewVertex,	-- Int -> Edge -> [Edge] -> Queue Node -> Queue Node -> Queue Node
	  showPath			-- Queue Node -> [Int]
	) where

import Graphs
import Queue
import Vertex

{-
	This fucntion gets a graph and gives gives the list of node
	value of which order this algorithm has explored vertices 
	in the given graph 
-}
bft 							:: [Edge] -> [Int]
bft []							= []
bft g							= reverse (showPath 
										(findEdge (fst1(head g)) g 
											(add (fst1(head g)) newQueue) 
											 (add (fst1(head g)) newQueue)))

-- Function: findEdge
{-
	This fucntion find an adjacent edge in order to explore
	the adjacent vertex.
-}
findEdge 								:: Node -> 
											[Edge] -> 
											Queue Node -> 
											Queue Node ->
											Queue Node
findEdge v1 g oq cq
	| isEmpty oq						= cq	-- An empty Queue
	| isEdgeExist v1 g	(showPath cq)	= exploreNewVertex 
											(getEdge v1 g (showPath cq)) g oq cq
	| otherwise							= findEdge 
											(top (snd(remove oq))) 
											g 
											(snd(remove oq))
											cq
-- Function: exploreNewVertex
{-
	This function examines a particular vertex.
-}
exploreNewVertex							:: Edge -> [Edge] -> 
												Queue Node -> 
												Queue Node -> 
												Queue Node
exploreNewVertex e g oq cq					= findEdge (top oq) 
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
