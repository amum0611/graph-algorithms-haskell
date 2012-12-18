# Introduction

This is a simple graph and greedy search algorithms implemented using Haskell. Following are main algorithms being implemented.

	1) Depth-First Search 
	2) Breadth-First Search
	3) Breadth-First Traversal
	4) Prim’s Algorithm
	5) Kruskal’s Algorithm

# How To:

Interpret MAIN.hs using any Haskell interpreter of your choice.

# Background

This Haskell program has implementation of Depth-First Search, Breadth-First Search, Breadth-First Traversal, Prim’s Algorithm, and Kruskal’s Algorithm. During the implementation, various Haskell prelude functions as well as Haskell techniques has been used throughout. Length, map, snd, fst, head, last, and tail are some of functions used that are packaged in Prelude module. Haskell has mechanism to implement various techniques to be used in programmers’ application. Such mechanisms are polymorphism, algebraic data types, abstract data types (ADT), list comprehensions, classes, primitive recursion, higher order functions, IO and monads, and etc.

When a graph and a goal node is given depth-first and breadth-first algorithms produces a list of nodes as the route to the goal node. If the goal node is not found, an empty list is showed. Prim’s and Kruskal’s algorithms solves the problem of minimum spanning trees. In order to check whether a fringe of tree forms a cycle with about to add edge, breadth-first traversal is used to determine this.

A graph is a list of edges. Each edge contains two adjacent vertices and the weight of the edge. Each vertex is an ADT. To keep the trace of traversal in all five algorithms, a special kind of data structures needed. Those are Stacks, Queues, and Priority Queues.

These data structures are implemented using the concept of ADT. This mechanism allows each data structures to hold any sort of data types both Haskell prelude data types as well as user defined algebraic data types. However, once these data structures were initialized with a particular data type, then it can only hold that particular data type.

Monads and IO in Haskell takes out the pureness in functional programming paradigm, because of program states are used during IO processing. However, these two mechanisms is used to create a menu system as a facade to all functionalities in this system. Nevertheless, all those functionalities are decoupled completely from monads and IO.

