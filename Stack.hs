---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search Algorithms
	
	Script		: Stack.hs - This abstract data type 
				  provides the data structure to be used
				  with any data types "a" by implementing
				  LIFO strategy
-}
---------------------------------------------------------
module Stack
	( Stack,
	  newStack,		-- Stack a
	  isEmpty,		-- Stack a -> Bool
	  push,			-- a -> Stack a -> Stack a
	  pop,			-- Stack a -> Stack a
	  top,			-- Stack a -> a
	  printData		-- Stack a -> [a]
	) where

-- A more efficient way to declare data types rather with one unary constructors
newtype Stack a	= MyStack [a] deriving(Eq, Ord, Show, Read)

-- Function: newStack
{-
	This function creates a brand new Stack
-}
newStack 		:: Stack a
newStack		= MyStack []

-- Function: isEmpty
{-
	Check whether the stack is empty
-}
isEmpty					:: Stack a -> Bool
isEmpty (MyStack [])	= True
isEmpty _				= False

-- Function: push
{-
	This function pushes a value into the stack
-}
push					:: a -> Stack a -> Stack a
push x (MyStack xs)		= MyStack (xs ++ [x])

-- Function: pop
{-
	This function pull the value from the stack
	that is pushed recently
-}
pop							:: Stack a -> Stack a
pop stack@(MyStack xs)	-- This pattern matching provides the input to be matched
						-- both stack and xs
	| not (isEmpty stack)	= MyStack (init xs)
	| otherwise				= error "Stack.pop: Stack is empty"
	
-- Function: top
{- 
	Returns the top value from the stack
-}
top						:: Stack a -> a
top (MyStack [])		= error "Stack.top: Stack is empty"
top (MyStack xs)		= last xs

-- Function: printData
{-
	This function converts the stack to a list
-}
printData					:: Stack a -> [a]
printData (MyStack [])		= []
printData (MyStack xs)		= [x |x <- xs]
