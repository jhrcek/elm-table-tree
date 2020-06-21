module TreeBuilder exposing (buildTree, drawTree)

import Html
import List
import MyTree exposing (MyTree)
import Svg exposing (Svg, circle, g, line, text, text_)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, textAnchor, transform, x1, x2, y1, y2)
import TreeDiagram exposing (Tree, defaultTreeLayout, leftToRight, node)
import TreeDiagram.Svg


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 "0", y1 "0", x2 (String.fromFloat targetX), y2 (String.fromFloat targetY), stroke "black" ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : String -> Svg msg
drawNode label =
    g
        []
        [ circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , text_ [ textAnchor "middle", transform "translate(0,5)" ] [ text label ]
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
    let
        xsss =
            groupByHead <| sortByHead xss
    in
    List.filterMap
        (\group ->
            let
                tailsToBeGrouped : List (List String)
                tailsToBeGrouped =
                    List.filterMap List.tail group
            in
            List.head group
                |> Maybe.andThen List.head
                |> Maybe.map (\label -> buildTree label tailsToBeGrouped)
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
                    if List.head xs == (List.head ys |> Maybe.andThen List.head) then
                        (xs :: ys) :: yss

                    else
                        [ xs ] :: (ys :: yss)
    in
    List.foldr f []
