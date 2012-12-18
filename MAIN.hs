---------------------------------------------------------
{-
	Name		: Azeem Mumtaz
	
	Title		: Search & Greedy Algorithms
	
	Script		: MAIN.hs - Main Page
				  This module acts as facade hence 
				  implements an interactive menu system.
-}
---------------------------------------------------------
module MAIN where

import DepthFirstSearch (dfs)
import BreadthFirstSearch (bfs)
import BreadthFirstTraversal (bft)
import PrimsAlgorithm (prims)
import KruskalsAlgorithm (kruskals)

import Graphs

-- Function: menu
menu 		:: IO ()
menu 		= 
	do
		header
		putStrLn "\n\tMenu"
		putStrLn "\n\t(1) Depth-First Search"
		putStrLn "\t(2) Breadth-First Search"
		putStrLn "\t(3) Breadth-First Traversal"
		putStrLn "\t(4) Prim's Algorithm"
		putStrLn "\t(5) Kruskal's Algorithm"
		putStrLn "\t(0) Exit"
		putStr "\n\t\tYour Selection: "
		option <- getOption
		if (isValid option 5)
		    then
				subMenuRouter option
			else
				do
					putStrLn "\tInvalid Selection"
					menu
				
-- Function: header
{-
	This fancy function displays the header in all sort of 
	menus
-}
header		:: IO ()
header		=
	do
		putStrLn "\t--------------------------"
		putStrLn "\tSearch & Greedy Algorithms"
		putStrLn "\t--------------------------"

-- Function: subMenuRouter
{-
	This menu routes through the submenus
-}
subMenuRouter		:: Int -> IO ()
subMenuRouter x
	| x == 1		= dfsMenu
	| x == 2		= bfsMenu
	| x == 3		= bftMenu
	| x == 4		= primsMenu
	| x == 5		= kruskalsMenu
	| otherwise		= putStrLn "\n\t\tThank You!"

-- Function: dfMenu
dfsMenu				:: IO ()
dfsMenu				= 
	do
		header
		putStrLn 	"\n\tMenu -> DF Search"
		putStr "\n\tEnter a Graph: "
		graph <- getGraph
		putStr "\tEnter a Goal Node: "
		goal <- getOption
		let result = (dfs graph goal) :: [Int]
		if (not (isEmpty result))
			then
				putStrLn ("\n\tResult: " ++ (show result))
			else
				do
					putStrLn "\n\tGoal Node is not Found"
		menu

-- Function: bfsMenu
bfsMenu				:: IO ()
bfsMenu				= 
	do
		header
		putStrLn 	"\n\tMenu -> BF Search"
		putStr "\n\tEnter a Graph: "
		graph <- getGraph
		putStr "\tEnter a Goal Node: "
		goal <- getOption
		let result = (bfs graph goal) :: [Int]
		if (not (isEmpty result))
			then
				putStrLn ("\n\tResult: " ++ (show result))
			else
				do
					putStrLn "\n\tGoal Node is not Found"
		menu

-- Function: bftMenu
bftMenu				:: IO ()
bftMenu				= 
	do
		header
		putStrLn 	"\n\tMenu -> BF Travesal"
		putStr "\n\tEnter a Graph: "
		graph <- getGraph
		let result = (bft graph) :: [Int]
		if (not (isEmpty result))
			then
				putStrLn ("\n\tResult: " ++ (show result))
			else
				do
					putStrLn "\n\tNo Traversal Trace Found"
		menu

-- Function: primsMenu
primsMenu			:: IO ()
primsMenu			= 
	do
		header
		putStrLn 	"\n\tMenu -> MST - Prims"
		putStr "\n\tEnter a Graph: "
		graph <- getGraph
		let result = (prims graph) :: [(Int, Int, Int)]
		if (not (isEmpty result))
			then
				putStrLn ("\n\tMinimum Spanning Tree: " ++ (show result))
			else
				do
					putStrLn "\n\tMST is not generated"
		menu
	
-- Function: kruskalsMenu
kruskalsMenu		:: IO ()
kruskalsMenu		= 
	do
		header
		putStrLn 	"\n\tMenu -> MST - Kruskals"
		putStr "\n\tEnter a Graph: "
		graph <- getGraph
		let result = (kruskals graph) :: [(Int, Int, Int)]
		if (not (isEmpty result))
			then
				putStrLn ("\n\tMinimum Spanning Tree: " ++ (show result))
			else
				do
					putStrLn "\n\tMST is not generated"
		menu
		
-- Function: isValid
{-
	This function will check whether the input is 
	between 0 and the specified maximum value
-}
isValid					:: Int -> Int -> Bool
isValid	a b
	| 0 <= a && a <= b	= True
	| otherwise			= False
	
-- Function: getOption
{-
	This function converts a string to int
-}
getOption 			:: IO Int
getOption 			= 
	do 
		s <- getLine
		return (read s :: Int)

-- Function: getGraph
{-
	This function converts a string to Graph
-}
getGraph 			:: IO [Edge]
getGraph 			= 
	do 
		s <- getLine
		let graph = ((buildGraph s) :: [Edge])
		return graph
		if (isEmptyG graph)
			then
				do
					error "Invalid graph entered.."
					return []
			else
				return graph
		
-- Function: isEmpty
isEmpty					:: [a] -> Bool
isEmpty s
	| (length s) == 0	= True
	| otherwise			= False
	
-- Function: isEmptyG
{-
	Checks whether a graph is empty or not
-}
isEmptyG				:: [Edge] -> Bool
isEmptyG s
	| (length s) == 0	= True
	| otherwise			= False
