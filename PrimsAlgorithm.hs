---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search & Greedy Algorithms
	
	Script		: PrimsAlgorithm.hs - This module has
				  implementation of finding minimum
				  spanning tree for a given graph
				  using Prim's Algorithm 
-}
---------------------------------------------------------
module PrimsAlgorithm
	( prims,				-- [Edge] -> [(Int,Int,Int)]
	  exploreShortestEdge,	-- [Edge] -> PQueue Edge -> PQueue Edge
	  getEligEdge,			-- [Edge] -> PQueue Edge -> PQueue Edge
	  findEdges,			-- [Edge] -> [Edge] -> PQueue Edge -> [Edge]
	  formsCycle,			-- Edge -> [Edge] -> Bool
	  searchForCycle,		-- Edge -> [Edge] -> [[Int]]
	  mapGraph,				-- ([Edge] -> Int -> [Int]) -> Node -> [[Edge]] -> [[Int]]
	  generateListOfGraphs,	-- Edge -> [Edge] -> [[Edge]]
	  explorerNewEdges,		-- PQueue Edge -> Edge -> [Edge] -> [Edge]
	  removeDuplicateEdges,	-- [Edge] -> [Edge] -> [Edge]
	  mapCycles,			-- (Edge -> [Edge] -> Bool) -> [Edge] -> [Edge] -> [Edge]
	  filterEdges,			-- (Edge -> Edge -> Bool) -> Edge -> [Edge] -> [Edge]
	  isNotDuplicate,		-- Edge -> Edge -> Bool
	  rule					-- [Edge] -> [Edge]
	) where

import Graphs
import PriorityQueue
import Vertex
import BreadthFirstTraversal (bft)

{-
	This fucntion gets a graph and generates a minimum
	spanning tree according to Prim's algorithm.
-}
prims 				:: [Edge] -> [(Int,Int,Int)]
prims []			= []
prims g				= printGraph(printData 
						(exploreShortestEdge 
						(visitGraph (head (rule g)) g) 
						(add rule (head (rule g)) newPQueue)))

-- Function: exploreShortestEdge
{-
	This function forms the minimum spanning tree, by adhering
	constraints.
-}
exploreShortestEdge			:: [Edge] -> PQueue Edge -> PQueue Edge
exploreShortestEdge g cq		-- cq = Closed PQueue
	| isEmpty cq						= newPQueue
	| ((vertexCount (printData cq)) /= 
	  (vertexCount g))					= exploreShortestEdge g (getEligEdge g cq)
	| otherwise							= cq

-- Function: getEligEdge
{-
	This function returns the eligible path from a priority 
	queue
-}
getEligEdge			:: [Edge] -> PQueue Edge -> PQueue Edge
getEligEdge g cq	= add rule (top(addAll rule 
									(removeCyclicEdges g cq) 
									newPQueue)) cq

-- Function: removeCyclicEdges
{-
	This fucntion removes edges that forms cycle in Minimum
	Spanning Tree
-}
removeCyclicEdges			:: [Edge] -> PQueue Edge -> [Edge]
removeCyclicEdges g cq		= mapCycles formsCycle (printData cq)
								(findEdges g (printData cq) cq)

-- Function: mapCycles
{-
	This higher order function filters outs the MOST eligible egdes
	that does not forms anny cycles out of all eligible edges
-}
mapCycles					:: (Edge -> [Edge] -> Bool) -> [Edge] -> [Edge] -> [Edge]
mapCycles f mst []			= []
mapCycles f mst (x:xs)
	| not(f x mst)			= [x] ++ (mapCycles f mst xs)
	| otherwise				= (mapCycles f mst xs)

-- Function: findEdges
{-
	This fucntion finds an adjacent edges of the minimum spanning
	tree in order to explore the adjacent vertex.
-}
findEdges				:: [Edge] -> [Edge] -> PQueue Edge -> [Edge]
findEdges g [] cq		= []
findEdges g (x:xs) cq	= (explorerNewEdges cq x g) ++
							  (findEdges g xs cq)

-- Function: formsCycle
{-
	This function examines an Edge whether it will form a cycle
	in the minimum spanning tree
-}	
formsCycle				:: Edge -> [Edge] -> Bool
formsCycle (v1,w,v2) []	= False
formsCycle (v1,w,v2) es	= isCycle (searchForCycle (v1,w,v2) es) (v1,w,v2)
							where
							isCycle						:: [[Int]] -> Edge -> Bool
							isCycle [] _				= False
							isCycle (l:ls) (v1,w,v2)
								| elem (get v1) l &&
									elem (get v2) l		= True
								| otherwise				= isCycle ls (v1,w,v2)

-- Function: searchForCycle
{-
	This fucntion returns list of searches from the given graph
	is applicable
-}
searchForCycle					:: Edge -> [Edge] -> [[Int]]
searchForCycle (v1,w,v2) []		= []
searchForCycle (v1,w,v2) es		= mapGraph
									bft
									(generateListOfGraphs (last es) es)

-- Function: mapGraph 
{-
	This higher order fucntion applies bft to each graphs
	in the list of graphs
-}
mapGraph				:: ([Edge] -> [Int]) -> [[Edge]] -> [[Int]]
mapGraph f []			= []
mapGraph f (g:gs)		= [f g] ++ (mapGraph f gs)

-- Function: generateListOfGraphs
{-
	This function will generate list of possible graphs
-}
generateListOfGraphs		:: Edge -> [Edge] -> [[Edge]]
generateListOfGraphs e es
	| e /= es!!0			= [es] ++ (generateListOfGraphs e ((tail es) ++ [head es]))
	| otherwise				= [es]

-- Function: explorerNewEdges
{-
	This function explores edges that are eligible to be a part of
	the minimum spanning tree
-}
explorerNewEdges				:: PQueue Edge -> Edge -> [Edge] -> [Edge]
explorerNewEdges cq (v1,w,v2) g	= removeDuplicateEdges (printData cq) 
									((getAllEdges v1 g []) ++ (getAllEdges v2 g []))

-- Function: removeDuplicateEdges
{-
	This function removes newly explored edges which are already in the
	queue.
-}
removeDuplicateEdges			:: [Edge] -> [Edge] -> [Edge]
removeDuplicateEdges (q:qs) es
	| (length qs) /= 0			= removeDuplicateEdges qs (filterEdges isNotDuplicate q es)
	| (length qs) == 0			= (filterEdges isNotDuplicate q es)

-- Function: filterEdges
{-
	This higher order function applies isNotDuplicate and an Edge 
	over a list of Egdes to filter out duplicates.
-}
filterEdges 		:: (Edge -> Edge -> Bool) -> Edge -> [Edge] -> [Edge]
filterEdges f q es	= [x | x <- es, f q x]

-- Function: isNotDuplicate
{-
	This function checks whether 2 edges are equals interms of
	their vertices.
-}
isNotDuplicate			:: Edge -> Edge -> Bool
isNotDuplicate (a,b,c) (x,y,z)
	| a == z && c == x		= False
	| a == x && c == z		= False
	| otherwise				= True

-- Function: rule
{-
	This function has the rule to prioritize edges that is
	passed as a parameter to priority queue, thus making the 
	priority queue genuienly abstract.
-}
rule				:: [Edge] -> [Edge]
rule g				= quickSortW g
