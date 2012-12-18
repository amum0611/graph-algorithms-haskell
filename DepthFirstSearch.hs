---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search Algorithms
	
	Script		: DepthFirstSearch.hs - This module has
				  implementation of Depth First Search
				  Algorithm
-}
---------------------------------------------------------
module DepthFirstSearch
	( dfs,						-- [Edge] -> Int -> [Int]
	  findEdge,					-- Int -> Node -> [Edge] -> Stack Node -> Stack Node
	  exploreNewVertex,			-- Int -> Edge -> [Edge] -> Stack Node -> Stack Node
	  isGoalFoundWithNoEdges,	-- Int -> Edge -> [Edge] -> Stack Node -> Bool
	  showPath					-- Stack Node -> [Int]
	) where

import Graphs
import Stack
import Vertex

{-
	This fucntion gets a graph and a goal node, and gives
	a list of nodes to get to that goal node. If a node is 
	not found, an empty list is produced. 
-}
dfs 								:: [Edge] -> Int -> [Int]
dfs [] _							= []
dfs g goal
	| get (fst1(head g)) == goal	= [goal]
	| otherwise						= showPath 
										(findEdge goal (snd1(head g)) 
											(visitGraph (head g) g) 
											(push (fst1(head g)) newStack))

-- Function: findEdge
{-
	This fucntion find an adjacent edge in order to explore
	deeper through that edge.
-}
findEdge 								:: Int -> Node -> 
											[Edge] -> 
											Stack Node -> 
											Stack Node
findEdge goal v1 g s
	| isEmpty s							= newStack -- An empty Queue
	| isEdgeExist v1 g	(showPath s)	= exploreNewVertex 
											goal (getEdge v1 g (showPath s)) g s
	| otherwise							= findEdge goal (top (pop s)) g (pop s)

-- Function: exploreNewVertex
{-
	This function examines a particular vertex whether it is 
	the goal node, or whether the adacent node is the goal node,
	or else whether this vertex is a part of an edge where
	there are no further adjacent edges.
-}
exploreNewVertex							:: Int -> Edge -> [Edge] -> 
												Stack Node -> 
												Stack Node
exploreNewVertex goal e g s
	| goal == (get(fst1 e))						= push (fst1 e) s
	| isGoalFoundWithNoEdges goal e g s			= push (snd1 e) (push (fst1 e) s)
	| not (isEdgeExist (snd1 e) g (showPath s))	= findEdge goal 
													(fst1 e) 
													(visitGraph e g) 
													(push (snd1 e) s)
	| otherwise									= findEdge goal 
													(snd1 e) 
													(visitGraph e g) 
													(push (fst1 e) s)

-- Function: isGoalFoundWithNoEdges
{-
	This fucntion checks whether the edges is the goal node and 
	it is a part of an edge where there are no adjacent edges
-}
isGoalFoundWithNoEdges						:: Int -> Edge -> [Edge] -> 
												Stack Node -> Bool
isGoalFoundWithNoEdges goal e g s
	| (goal == (get(snd1 e))) && 
		not (isEdgeExist (snd1 e) g (showPath s))	= True
	| otherwise										= False

-- Function: showPath
{-
	This fucntion gets all values from the Stack
-}
showPath			:: Stack Node -> [Int]
showPath s			= [get x | x <- (printData s)]

-- END
