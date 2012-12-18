---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search Algorithms
	
	Script		: Vertex.hs - This Abstract data type
				  builds the data type "Vertex" which 
				  represent a node in a graph
-}
---------------------------------------------------------
	
	module Vertex
		( Vertex,
		  newVertex,	-- a -> Vertex (a, Bool)
		  visit,		-- Vertex (a, Bool) -> Vertex (a, Bool)
		  isVisited,	-- Vertex (a, Bool) -> Bool
		  get			-- Vertex (a, Bool) -> a
		) where

-- A more efficient way to declare data types rather with one unary constructors
newtype Vertex a 	= MyVertex (a, Bool) deriving(Eq, Show, Read)

-- Function: visit
{-
	Changes the boolean value of the graph to True that denotes
	this vertex is visited by any search algorithm
-}
visit						:: Vertex (a, Bool) -> Vertex (a, Bool)
visit (MyVertex (x, _))		= MyVertex (x, True)

-- Function: isVisited
{-
	This function check whether a Vertex is visited
-}
isVisited					:: Vertex (a, Bool) -> Bool
isVisited (MyVertex (x, y))
	| y == True				= True
	| otherwise				= False

-- Function: get
{-
	This function returns the value of the vertex
-}
get							:: Vertex (a, Bool) -> a
get (MyVertex pair)			= fst (fst pair)

-- Function: newVertex
{- 
	This function creates a new vertex when a node value is 
	given
-}
newVertex					:: a -> Vertex (a, Bool)
newVertex x					= MyVertex ((x, False), False)
