module TreeBuilder exposing (drawTree, buildTree, groupByHead)

import Svg exposing (Svg, svg, circle, line, g, text', text)
import Svg.Attributes exposing (..)
import TreeDiagram exposing (node, Tree, defaultTreeLayout, leftToRight)
import TreeDiagram.Svg
import Html
import List


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 "0", y1 "0", x2 (toString targetX), y2 (toString targetY), stroke "black" ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : String -> Svg msg
drawNode label =
    g
        []
        [ circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , text' [ textAnchor "middle", transform "translate(0,5)" ] [ text label ]
        ]


drawTree : Tree String -> Html.Html msg
drawTree t =
    let
        customLayout =
            { defaultTreeLayout | orientation = leftToRight }
    in
        TreeDiagram.Svg.draw customLayout drawNode drawLine t


buildTree : String -> List (List String) -> Tree String
buildTree rootLabel xs =
    node rootLabel (buildForest xs)


buildForest : List (List String) -> List (Tree String)
buildForest xss =
    let
        xsss =
            groupByHead <| sortByHead xss
    in
        List.filterMap
            (\group ->
                let
                    groupLabel =
                        Maybe.andThen (List.head group) List.head

                    tailsToBeGrouped : List (List String)
                    tailsToBeGrouped =
                        List.filterMap List.tail group
                in
                    case groupLabel of
                        --dealing with empty lists
                        Nothing ->
                            Nothing

                        Just label ->
                            Just (buildTree label tailsToBeGrouped)
            )
            xsss


sortByHead : List (List String) -> List (List String)
sortByHead =
    List.sortBy (\xs -> Maybe.withDefault "can't happen" <| List.head xs)


groupByHead : List (List a) -> List (List (List a))
groupByHead =
    let
        f xs acc =
            case acc of
                [] ->
                    [ [ xs ] ]

                ys :: yss ->
                    if List.head xs == ((List.head ys) `Maybe.andThen` List.head) then
                        ((xs :: ys) :: yss)
                    else
                        [ xs ] :: (ys :: yss)
    in
        List.foldr f []
