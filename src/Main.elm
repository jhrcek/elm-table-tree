module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import MyTree
import String
import Task
import TreeBuilder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type alias Cells =
    Dict ( Int, Int ) String


type alias Records =
    List (List String)


type alias Model =
    { cells : Cells
    , rowCount : Int
    , columnCount : Int
    , editedCell : Maybe ( Int, Int )
    }


type Msg
    = CellTyped Int Int String
    | CellClicked Int Int
    | CellLostFocus
    | BlobPasted String
    | ChangeRowCount Int
    | ChangeColumnCount Int
    | SwapColumns Int Int
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = blobToCells initialBlob
      , rowCount = 4
      , columnCount = 4
      , editedCell = Nothing
      }
    , Cmd.none
    )


initialBlob : String
initialBlob =
    """Jan,33,Tester,male
Jane,40,Shop assistant,female
Dan,60,Professor,male
Lydia,40,Clerk,female"""


getValueAt : Int -> Int -> Cells -> String
getValueAt row col cells =
    Dict.get ( row, col ) cells |> Maybe.withDefault ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlobPasted blob ->
            ( { model | cells = blobToCells blob }, Cmd.none )

        CellClicked row col ->
            ( { model | editedCell = Just ( row, col ) }
            , Task.attempt (always NoOp) (Dom.focus (toCellId row col))
            )

        CellLostFocus ->
            ( { model | editedCell = Nothing }, Cmd.none )

        CellTyped row col str ->
            ( { model | cells = Dict.insert ( row, col ) str model.cells }, Cmd.none )

        ChangeRowCount delta ->
            ( { model | rowCount = model.rowCount + delta }, Cmd.none )

        ChangeColumnCount delta ->
            ( { model | columnCount = model.columnCount + delta }, Cmd.none )

        SwapColumns col1 col2 ->
            ( { model | cells = swapColumns col1 col2 model.cells }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


swapColumns : Int -> Int -> Cells -> Cells
swapColumns from to cells =
    let
        swapColIndex ( ( a, b ), val ) =
            ( ( a
              , if b == from then
                    to

                else if b == to then
                    from

                else
                    b
              )
            , val
            )
    in
    Dict.toList cells |> List.map swapColIndex |> Dict.fromList


recordsToCells : Records -> Cells
recordsToCells records =
    let
        maximumLength =
            Maybe.withDefault 0 <| List.maximum <| List.map List.length records

        paddedRecords =
            List.map (\xs -> xs ++ List.repeat (maximumLength - List.length xs) "") records
    in
    List.indexedMap (\rowIdx record -> List.indexedMap (\colIdx cellValue -> ( ( rowIdx, colIdx ), cellValue )) record) paddedRecords
        |> List.concat
        |> Dict.fromList


blobToCells : String -> Cells
blobToCells =
    blobToRecords >> recordsToCells


cellsToRecords : Int -> Int -> Cells -> Records
cellsToRecords rowCount columnCount cells =
    List.map
        (\row ->
            List.map
                (\column ->
                    Maybe.withDefault "" <| Dict.get ( row, column ) cells
                )
                (List.range 0 (columnCount - 1))
        )
        (List.range 0 (rowCount - 1))


blobToRecords : String -> Records
blobToRecords str =
    let
        rows =
            String.split "\n" str
    in
    List.map (String.split ",") rows


recordsToBlob : Records -> String
recordsToBlob records =
    String.join "\n" <| List.map (String.join ",") records



--- VIEW


view : Model -> Html Msg
view model =
    let
        renderTable =
            Html.table [] <|
                renderHeader
                    :: List.map renderRow (List.range 0 (model.rowCount - 1))

        renderHeader =
            Html.tr [] <| List.map renderHeaderCell <| List.range 0 (model.columnCount - 1)

        renderHeaderCell col =
            Html.th []
                ((if col > 0 then
                    [ Html.span [ Events.onClick <| SwapColumns col (col - 1) ] [ Html.text "◀ " ] ]

                  else
                    []
                 )
                    ++ (if col < model.columnCount - 1 then
                            [ Html.span [ Events.onClick <| SwapColumns col (col + 1) ] [ Html.text " ▶" ] ]

                        else
                            []
                       )
                )

        renderRow row =
            Html.tr [] <| List.map (renderCell row) <| List.range 0 (model.columnCount - 1)

        renderCell row col =
            Html.td
                [ Events.onClick (CellClicked row col)
                , A.style "width" <| (String.fromInt <| 100 // model.columnCount) ++ "%"
                ]
                [ renderValue row col ]

        renderValue row col =
            let
                valString =
                    getValueAt row col model.cells
            in
            if model.editedCell == Just ( row, col ) then
                Html.input
                    [ Events.onInput <| CellTyped row col
                    , Events.onBlur CellLostFocus
                    , A.id <| toCellId row col
                    , A.value valString
                    , A.style "display" "table-cell"
                    , A.style "width" "99%"
                    ]
                    []

            else
                Html.text valString

        records =
            cellsToRecords model.rowCount model.columnCount model.cells

        myTree =
            TreeBuilder.buildTree "<root>" records
    in
    Html.div []
        [ Html.h2 [] [ Html.text "CSV Data" ]
        , Html.textarea [ Events.onInput BlobPasted, A.rows model.rowCount, A.cols 40, A.value (recordsToBlob records), A.placeholder "Paste CSV data here ..." ] []
        , Html.h2 [] [ Html.text "Parsed Data" ]
        , tableControls model.rowCount model.columnCount
        , renderTable
        , Html.h2 [] [ Html.text <| "Tree View (" ++ String.fromInt (MyTree.size myTree) ++ " nodes)" ]
        , TreeBuilder.drawTree myTree
        ]


tableControls : Int -> Int -> Html Msg
tableControls rowCount colCount =
    let
        but msg txt =
            Html.button [ Events.onClick msg ] [ Html.text txt ]

        createButtonIf cond btn =
            if cond then
                [ btn ]

            else
                []

        addRow =
            createButtonIf (rowCount < 11)
                (but (ChangeRowCount 1) "Add row")

        removeRow =
            createButtonIf (rowCount > 0)
                (but (ChangeRowCount -1) "Remove row")

        addColumn =
            createButtonIf (colCount < 11)
                (but (ChangeColumnCount 1) "Add column")

        removeColumn =
            createButtonIf (colCount > 0)
                (but (ChangeColumnCount -1) "Remove column")
    in
    Html.div [] (List.concat [ addRow, removeRow, addColumn, removeColumn ])


toCellId : Int -> Int -> String
toCellId row col =
    String.fromInt row ++ ":" ++ String.fromInt col
