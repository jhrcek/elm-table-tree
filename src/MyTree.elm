module MyTree exposing
    ( MyTree(..)
    , size
    , toDiagramTree
    )

import TreeDiagram


{-| This module exists because TreeDiagram.Tree is opaque, but I need access to its structure
to calculate e.g. the number of nodes.

So then parsing table into Tree I go from `List (List String)` to `MyTree String` to `TreeDiagram.Tree`.

-}
type MyTree a
    = MyNode a (List (MyTree a))


size : MyTree a -> Int
size (MyNode _ children) =
    1 + List.sum (List.map size children)


toDiagramTree : MyTree a -> TreeDiagram.Tree a
toDiagramTree (MyNode a children) =
    TreeDiagram.node a (List.map toDiagramTree children)
