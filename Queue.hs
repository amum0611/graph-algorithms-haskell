---------------------------------------------------------
{-
	Module		: Advance Programming Language & Concept
	Batch		: GF1041SE
	Name		: Azeem Mumtaz
	CB Number	: CB003033
	
	Title		: Search Algorithms
	
	Script		: Queue.hs - This abstract data type 
				  provides the data structure to be used
				  with any data types "a" by implementing
				  FIFO strategy.
-}
---------------------------------------------------------
module Queue
	( Queue,
	  newQueue,		-- Queue a
	  isEmpty,		-- Queue a -> Bool
	  add,			-- a -> Queue a -> Queue a
	  remove,		-- Queue a -> (a, Queue a)
	  top,			-- Queue a -> a
	  printData		-- Queue a -> [a]
	) where

-- A more efficient way to declare data types rather with one unary constructors
newtype Queue a = MyQueue [a] deriving(Eq, Ord, Show, Read)

-- Function: newQueue
{-
	This function creates a brand new Queue
-}
newQueue 		:: Queue a
newQueue		= MyQueue []

-- Function: isEmpty
{-
	Check whether the Queue is empty
-}
isEmpty					:: Queue a -> Bool
isEmpty (MyQueue [])	= True
isEmpty _				= False

-- Function: add
{-
	This function added a value into the queue
	from the rear
-}
add						:: a -> Queue a -> Queue a
add	x (MyQueue xs)		= MyQueue(x:xs)

-- Function: remove
{-
	This function removes the value from the Queue
	that is added at most earliest
-}
remove						:: Queue a -> (a, Queue a)
remove queue@(MyQueue xs)
	| not (isEmpty queue)	= (last xs, MyQueue (init xs))
	| otherwise 			= error "Queue is empty"
	
-- Function: top
{- 
	Returns the top value from the Queue
-}
top						:: Queue a -> a
top (MyQueue xs)		= last xs

-- Function: printData
{-
	This function converts the Queue to a list
-}
printData					:: Queue a -> [a]
printData (MyQueue [])		= []
printData (MyQueue xs)		= [x |x <- xs]