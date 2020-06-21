module TreeBuilder exposing (buildTree, drawTree)

import Html
import List
import List.Extra as List
import MyTree exposing (MyTree)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, textAnchor, transform, x1, x2, y1, y2)
import TreeDiagram exposing (defaultTreeLayout, leftToRight)
import TreeDiagram.Svg


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ x1 "0", y1 "0", x2 (String.fromFloat targetX), y2 (String.fromFloat targetY), stroke "black" ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : String -> Svg msg
drawNode label =
    Svg.g
        []
        [ Svg.circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , Svg.text_ [ textAnchor "middle", transform "translate(0,5)" ] [ Svg.text label ]
        ]


drawTree : MyTree String -> Html.Html msg
drawTree t =
    let
        customLayout =
            { defaultTreeLayout | orientation = leftToRight }
    in
    TreeDiagram.Svg.draw customLayout drawNode drawLine <| MyTree.toDiagramTree t


buildTree : String -> List (List String) -> MyTree String
buildTree rootLabel xs =
    MyTree.MyNode rootLabel (buildForest xs)


buildForest : List (List String) -> List (MyTree String)
buildForest xss =
    xss
        |> sortByHead
        |> groupByHead
        |> List.filterMap
            (\( first, rest ) ->
                List.head first
                    |> Maybe.map
                        (\label ->
                            let
                                tailsToBeGrouped : List (List String)
                                tailsToBeGrouped =
                                    List.filterMap List.tail <| first :: rest
                            in
                            buildTree label tailsToBeGrouped
                        )
            )


sortByHead : List (List String) -> List (List String)
sortByHead =
    List.sortBy (\xs -> Maybe.withDefault "" <| List.head xs)


groupByHead : List (List a) -> List ( List a, List (List a) )
groupByHead =
    List.groupWhile (\xs ys -> List.head xs == List.head ys)
