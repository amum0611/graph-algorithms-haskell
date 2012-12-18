---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search & Greedy Algorithms
	
	Script		: Graphs.hs - This includes functions 
				  relationed to manipulate Graphs
-}
---------------------------------------------------------
module Graphs
	( Node,					-- Vertex (Int, Bool)
	  Weight,				-- Int
	  Edge,					-- (Node, Weight, Node)
	  isEdgeExist,			-- Node -> [Edge] -> [Int] -> Bool
	  getEdge,				-- Node -> [Edge] -> [Int] -> Edge
	  getAllEdges,			-- Node -> [Edge] -> [Int] -> [Edge]
	  visitGraph,			-- Edge -> [Edge] -> [Edge]
	  makeEdgeVisited,		-- Edge -> Edge
	  isExistInList,		-- Int -> [Int] -> Bool
	  makeUnDirected,		-- [Edge] -> [Edge]
	  quickSort,			-- [Edge] -> [Edge]
	  quickSortW,			-- [Edge] -> [Edge]
	  edgeCount,			-- [Edge] -> Int
	  vertexCount,			-- [Edge] -> Int
	  printGraph,			-- [Edge] -> [(Int, Int, Int)]
	  fst1,					-- (a,b,c) -> a
	  mdl1,					-- (a,b,c) -> b
	  snd1,					-- (a,b,c) -> c
	  buildGraph,			-- String -> [Edge]
	  convertGraphToVertex,	-- [Edge] -> [Int]
	  
	  -- Sample Graphs
	  petersenGraph1, petersenGraph2,
	  tree1, tree2, graph1, graph2, 
	  graph3, graph4
	) where

import Vertex
import List (nub, unzip3)

type Node			= Vertex (Int, Bool)
type Weight			= Int
type Edge 			= (Node, Weight, Node)

-- Function: isEdgeExist
{- 	
	This function checks whether an Edge is available with a starting vertex
	in the given Graph
-}
isEdgeExist					:: Node -> [Edge] -> [Int] -> Bool
isEdgeExist v g xs
	| elem True [True | (v1, w, v2) <- g, 
						get v1 == get v && 
						isVisited v1 == False &&
						not (isExistInList (get v2) xs)]	= True
	| otherwise												= False

-- Function: getEdge
{-	
	This function returns an Edge from a graph where starting vertex should
	match with the inputed vertex. Further this vertex should already visited.
	As an assumption this fuction returns the edge that is not visited with the 
	least node value. Function "QuickSort" is used to get the least value
-}
getEdge						:: Node -> [Edge] -> [Int] -> Edge
getEdge v g xs				= head (getAllEdges v g xs)

-- Function: getAllEdges
{-	
	This function returnsa set of Edges from a graph where starting vertex should
	match with the inputed vertex. Further this vertex should already visited.
	As an assumption this fuction returns the edge that is not visited with the 
	least node value. Function "QuickSort" is used to get the least value
-}
getAllEdges					:: Node -> [Edge] -> [Int] -> [Edge]
getAllEdges v g xs			= quickSort [(v1, w, v2) | (v1, w, v2) <- g, 
									get v1 == get v && 
									isVisited v1 == False &&
									not (isExistInList (get v2) xs)]

-- Function: visitGraph
{-
	This function make an Edge in the graph as visited
-}
visitGraph					:: Edge -> [Edge] -> [Edge]
visitGraph edge g			= map evaluate g
								where
								evaluate 			:: Edge -> Edge
								evaluate e
									| edge == e		= makeEdgeVisited e
									| otherwise		= e
									
-- Function: makeEdgeVisited
{-
	This function makes an Edge visited by changing the boolean 
	value of the first vertex
-}
makeEdgeVisited				:: Edge -> Edge
makeEdgeVisited (v1, w, v2)	= (visit v1, w, v2)

-- Function: isExistInList
{-
	This fucntion check whether a node value exist in the list
-}
isExistInList				:: Int -> [Int] -> Bool
isExistInList _ []			= False 
isExistInList a xs			= elem a xs

-- Function: makeUnDirected
{-
	This function will make a graph to a undirected graph
-}
makeUnDirected		:: [Edge] -> [Edge]
makeUnDirected g	= quickSortW([e1 | e1 <- g] ++ [(z,y,x)| (x,y,z) <- g])

-- Function: QuickSort
{-
	When a graph is given, this function will sort that graph
	ascending order from the 2nd vertex
-}
quickSort				:: [Edge] -> [Edge]
quickSort []			= []
quickSort (x:xs) 		= quickSort [y | y <- xs, get(snd1 y) <= get(snd1 x)] 
							++ [x] 
							++ quickSort[y | y <- xs, get(snd1 y) > get(snd1 x)]

-- Function: quickSortW
{-
	When a graph is given, this function will sort that graph
	ascending order from the Weight
-}
quickSortW				:: [Edge] -> [Edge]
quickSortW []			= []
quickSortW (x:xs) 		= quickSortW [y | y <- xs, (mdl1 y) <= (mdl1 x)] 
							++ [x] 
							++ quickSortW[y | y <- xs, (mdl1 y) > (mdl1 x)]
														
-- Function: edgeCount
{-
	This function gives the number of edges in a given 
	graph
-}
edgeCount					:: [Edge] -> Int
edgeCount g					= length g

-- Function: vertexCount
{-
	This function gives the number of vertices in a given 
	graph
-}
vertexCount				:: [Edge] -> Int
vertexCount	g			= length (convertGraphToVertex g)

-- Function: convertGraphToVertex
{-
	This function gives the list of all vertices in a graph
-}
convertGraphToVertex	:: [Edge] -> [Int]
convertGraphToVertex g	= (combine(unzip3[(get x, w, get y) | (x,w,y) <- g]))
							where 
							combine				:: ([Int], [Int], [Int]) -> [Int]
							combine (xs,ws,ys)	= nub(concat[xs ++ ys])

-- Function: printGraph
{-
	This function will print the graph in the 
	format of (Vertex, Weigth, Vertex)
-}
printGraph					:: [Edge] -> [(Int, Int, Int)]
printGraph g				= [(get x, w, get y) | (x, w, y) <- g]

-- Function: fst1
{-
	The first element in a tupple that contains
	3 types
-}
fst1						:: (a,b,c) -> a
fst1 (a,b,c)				= a

-- Function: mdl1
{-
	The middle element in a tupple that contains
	3 types
-}
mdl1						:: (a,b,c) -> b
mdl1 (a,b,c)				= b

-- Function: snd1
{-
	The last element in a tupple that contains
	3 types
-}
snd1						:: (a,b,c) -> c
snd1 (a,b,c)				= c

-- Function: buildGraph
{-
	This function will assist in IO operations to convert
	a string to a graph
-}
buildGraph					:: String -> [Edge]
buildGraph s
	| s == "petersenGraph1"	= petersenGraph1
	| s == "petersenGraph2" = petersenGraph2
	| s == "tree1"			= tree1
	| s == "tree2"			= tree2
	| s == "graph1"			= graph1
	| s == "graph2"			= graph2
	| s == "graph3"			= graph3
	| s == "graph4"			= graph4
	| otherwise				= []
	
-- END

---------------------------------------------------------
-- Sample Graphs
---------------------------------------------------------

-- Peterson Graph No 1
petersenGraph1		:: [Edge]
petersenGraph1		= [(newVertex 0, 10, newVertex 1), (newVertex 1, 10, newVertex 0), (newVertex 1, 8, newVertex 2), 
					   (newVertex 2, 8, newVertex 1), (newVertex 2, 12, newVertex 3), (newVertex 3, 12, newVertex 2), 
					   (newVertex 3, 14, newVertex 4), (newVertex 4, 14, newVertex 3), (newVertex 4, 11, newVertex 0), 
					   (newVertex 0, 11, newVertex 4), (newVertex 5, 3, newVertex 6), (newVertex 6, 3, newVertex 5), 
					   (newVertex 6, 1, newVertex 7), (newVertex 7, 1, newVertex 6), (newVertex 7, 3, newVertex 8), 
					   (newVertex 8, 3, newVertex 7), (newVertex 8, 5, newVertex 9), (newVertex 9, 5, newVertex 8),
					   (newVertex 9, 8, newVertex 5), (newVertex 5, 8, newVertex 9), (newVertex 5, 17, newVertex 0), 
					   (newVertex 0, 17, newVertex 5), (newVertex 6, 9, newVertex 2), (newVertex 2, 9, newVertex 6), 
					   (newVertex 7, 8, newVertex 4), (newVertex 4, 8, newVertex 7), (newVertex 8, 3, newVertex 1), 
					   (newVertex 1, 3, newVertex 8), (newVertex 9, 1, newVertex 3), (newVertex 3, 1, newVertex 9)]

-- Peterson Graph No 2
petersenGraph2		:: [Edge]
petersenGraph2		= [(newVertex 0, 10, newVertex 1), (newVertex 1, 8, newVertex 2), 
					   (newVertex 2, 12, newVertex 3), 
					   (newVertex 3, 14, newVertex 4), (newVertex 4, 11, newVertex 0), 
					   (newVertex 5, 3, newVertex 6), 
					   (newVertex 6, 1, newVertex 7), (newVertex 7, 3, newVertex 8), 
					   (newVertex 8, 5, newVertex 9), 
					   (newVertex 9, 8, newVertex 5), (newVertex 5, 17, newVertex 0), 
					   (newVertex 6, 9, newVertex 2), 
					   (newVertex 7, 8, newVertex 4), (newVertex 8, 3, newVertex 1), 
					   (newVertex 9, 1, newVertex 3)]
					   
-- Tree 1: Directed
tree1				:: [Edge]
tree1				= [(newVertex 1, 10, newVertex 2), (newVertex 2, 6, newVertex 5), (newVertex 5, 2, newVertex 6),
					   (newVertex 5, 2, newVertex 7), (newVertex 2, 5, newVertex 8), (newVertex 1, 4, newVertex 3),
					   (newVertex 3, 1, newVertex 9), (newVertex 3, 9, newVertex 10), (newVertex 1, 8, newVertex 4),
					   (newVertex 4, 4, newVertex 11), (newVertex 11, 12, newVertex 12), (newVertex 11, 1, newVertex 13)]
					   
-- Tree 2: Undirected
tree2				:: [Edge]
tree2				= [(newVertex 1, 10, newVertex 2), (newVertex 2, 10, newVertex 1), (newVertex 2, 6, newVertex 5), 
					   (newVertex 5, 6, newVertex 2), (newVertex 5, 2, newVertex 6), (newVertex 6, 2, newVertex 5), 
					   (newVertex 5, 2, newVertex 7), (newVertex 7, 2, newVertex 5), (newVertex 2, 5, newVertex 8), 
					   (newVertex 8, 5, newVertex 2), (newVertex 1, 4, newVertex 3), (newVertex 3, 4, newVertex 1), 
					   (newVertex 3, 1, newVertex 9), (newVertex 9, 1, newVertex 3), (newVertex 3, 9, newVertex 10), 
					   (newVertex 10, 9, newVertex 3), (newVertex 1, 8, newVertex 4), (newVertex 4, 8, newVertex 1), 
					   (newVertex 4, 4, newVertex 11), (newVertex 11, 4, newVertex 4), (newVertex 11, 12, newVertex 12), 
					   (newVertex 12, 12, newVertex 11), (newVertex 13, 1, newVertex 11), (newVertex 11, 1, newVertex 13)]

-- Graph 1
graph1				:: [Edge]
graph1				= [(newVertex 0, 5, newVertex 1), (newVertex 1, 5, newVertex 0), (newVertex 1, 6, newVertex 2), 
					   (newVertex 2, 6, newVertex 1), (newVertex 0, 2, newVertex 9), (newVertex 9, 2, newVertex 0), 
					   (newVertex 1, 4, newVertex 3), (newVertex 3, 4, newVertex 1), (newVertex 3, 3, newVertex 9), 
					   (newVertex 9, 3, newVertex 3)]

-- Graph 2: Directed
graph2				:: [Edge]
graph2				= [(newVertex 0, 5, newVertex 1), (newVertex 1, 6, newVertex 2), (newVertex 0, 2, newVertex 9), 
					   (newVertex 1, 4, newVertex 3)]

-- Graph 3: Undirected
graph3				:: [Edge]
graph3				= [(newVertex 0, 6, newVertex 1), (newVertex 1, 6, newVertex 0), 
					   (newVertex 0, 5, newVertex 2), (newVertex 2, 5, newVertex 0),
					   (newVertex 1, 9, newVertex 2), (newVertex 2, 9, newVertex 1),
					   (newVertex 1, 13, newVertex 4), (newVertex 4, 13, newVertex 1),
					   (newVertex 2, 16, newVertex 3), (newVertex 3, 16, newVertex 2),
					   (newVertex 2, 12, newVertex 5), (newVertex 5, 12, newVertex 2),
					   (newVertex 4, 15, newVertex 3), (newVertex 3, 15, newVertex 4),
					   (newVertex 4, 8, newVertex 6), (newVertex 6, 8, newVertex 4),
					   (newVertex 3, 7, newVertex 5), (newVertex 5, 7, newVertex 3),
					   (newVertex 5, 3, newVertex 6), (newVertex 6, 3, newVertex 5)]
	
-- Graph 4: Directed
graph4				:: [Edge]
graph4				= [(newVertex 0, 6, newVertex 1), 
					   (newVertex 0, 5, newVertex 2),
					   (newVertex 1, 9, newVertex 2),
					   (newVertex 1, 13, newVertex 4),
					   (newVertex 2, 16, newVertex 3),
					   (newVertex 2, 12, newVertex 5),
					   (newVertex 4, 15, newVertex 3),
					   (newVertex 4, 8, newVertex 6),
					   (newVertex 3, 7, newVertex 5),
					   (newVertex 5, 3, newVertex 6)]				   
-- END
