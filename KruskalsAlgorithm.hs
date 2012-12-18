---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search & Greedy Algorithms
	
	Script		: KruskalsAlgorithm.hs - This module has
				  implementation of finding minimum
				  spanning tree for a given graph
				  using kruskal's Algorithm 
-}
---------------------------------------------------------
module KruskalsAlgorithm
	( kruskals,				-- [Edge] -> [(Int,Int,Int)]
	  exploreShortestEdge,	-- PQueue Edge -> PQueue Edge -> PQueue Edge
	  isNotInMst,			-- Edge -> [Edge] -> Bool
	  rule					-- [Edge] -> [Edge]
	) where

import Graphs
import PriorityQueue
import Vertex
import PrimsAlgorithm (filterEdges, isNotDuplicate, formsCycle)

{-
	This fucntion gets a graph and generates a minimum
	spanning tree according to Kruskal's algorithm.
-}
kruskals 				:: [Edge] -> [(Int,Int,Int)]
kruskals []				= []
kruskals g				= printGraph(printData(exploreShortestEdge
							(snd(remove(addAll rule g newPQueue)))
							(add rule (head (rule g)) newPQueue)))

-- Function: exploreShortestEdge
{-
	This function forms the minimum spanning tree, by adhering
	constraints related to Krushkal.
-}
exploreShortestEdge					:: PQueue Edge -> PQueue Edge -> PQueue Edge
exploreShortestEdge oq cq
	| isEmpty oq									= cq
	| isNotInMst (top oq) (printData cq) &&
		not (formsCycle (top oq) (makeUnDirected (printData cq))) 		
													= exploreShortestEdge 
														(snd (remove oq))
														(add rule (top oq) cq)
	| otherwise 									= exploreShortestEdge 
														(snd (remove oq)) cq

-- Function: isNotInMst
{-
	This function checks whether a purticular edge is already in the
	MST. Returns True if not.
-}
isNotInMst						:: Edge -> [Edge] -> Bool
isNotInMst e cs
	| length (filterEdges isNotDuplicate e cs)
		/= length cs			= False
	| otherwise					= True

-- Function: rule
{-
	This function has the rule to prioritize edges that is
	passed as a parameter to priority queue, thus making the 
	priority queue genuienly abstract.
-}
rule				:: [Edge] -> [Edge]
rule g				= quickSortW g
