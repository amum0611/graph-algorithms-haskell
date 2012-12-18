---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search & Greedy Algorithms
	
	Script		: PriorityQueue.hs - This abstract data 
				  type provides the data structure to be 
				  used with any data types "a" by 
				  implementing FIFO strategy with 
				  specified priority rules. 
-}
---------------------------------------------------------
module PriorityQueue
	( PQueue,
	  newPQueue,	-- PQueue a
	  isEmpty,		-- PQueue a -> Bool
	  add,			-- ([a] -> [a]) -> a -> PQueue a -> PQueue a
	  addAll,		-- ([a] -> [a]) -> [a] -> PQueue a -> PQueue a
	  remove,		-- PQueue a -> (a, PQueue a) 
	  top,			-- PQueue a -> a
	  printData		-- PQueue a -> [a]
	) where

-- A more efficient way to declare data types rather with one unary constructors
newtype PQueue a 	= MyPQueue [a] deriving(Eq, Ord, Show, Read)

-- Function: newQueue
{-
	This function creates a brand new PQueue
-}
newPQueue 		:: PQueue a
newPQueue		= MyPQueue []

-- Function: isEmpty
{-
	Check whether the PQueue is empty
-}
isEmpty					:: PQueue a -> Bool
isEmpty (MyPQueue [])	= True
isEmpty _				= False

-- Function: add
{-
	This function added a value into the queue
	from the rear
-}
add						:: ([a] -> [a]) -> a -> PQueue a -> PQueue a
add	f x (MyPQueue xs)	=  listToPQueue (f (printData (MyPQueue(x:xs))))

-- Function: addAll
{-
	This function added all values into the queue
	from the rear
-}
addAll							:: ([a] -> [a]) -> [a] -> PQueue a -> PQueue a
addAll f [] (MyPQueue xs)		= MyPQueue xs
addAll f (y:ys) (MyPQueue xs)	= addAll f ys (add f y (MyPQueue xs))

-- Function: remove
{-
	This function removes the value from the PQueue
	that is added at most earliest
-}
remove						:: PQueue a -> (a, PQueue a)
remove queue@(MyPQueue xs)
	| not (isEmpty queue)	= (last xs, MyPQueue (init xs))
	| otherwise 			= error "PQueue is empty"
	
-- Function: top
{- 
	Returns the top value from the PQueue
-}
top						:: PQueue a -> a
top (MyPQueue xs)		= last xs

-- Function: printData
{-
	This function converts the PQueue to a list
-}
printData					:: PQueue a -> [a]
printData (MyPQueue [])		= []
printData (MyPQueue xs)		= [x |x <- xs]

-- Function: listToPQueue
{-
	This function converts the list to PQueue
-}
listToPQueue			:: [a] -> PQueue a
listToPQueue xs			= MyPQueue (reverse xs)
