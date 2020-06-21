module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import List.Extra as List
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
    { parsedGrid : Result ParseError Grid
    , csv : String
    }


type ParseError
    = NotEnoughRows
    | NumberOfColumnsNotTheSame (List Int)


type alias Grid =
    { cells : Cells
    , rowCount : Int
    , columnCount : Int
    , editedCell : Maybe ( Int, Int )
    }


type Msg
    = CellTyped Int Int String
    | CellClicked Int Int
    | CellLostFocus
    | CsvInputChanged String
    | ChangeRowCount Int
    | ChangeColumnCount Int
    | SwapColumns Int Int
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { parsedGrid = parseGrid initCsv
      , csv = initCsv
      }
    , Cmd.none
    )


initCsv : String
initCsv =
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
        CsvInputChanged csv ->
            ( { model
                | csv = csv
                , parsedGrid = parseGrid csv
              }
            , Cmd.none
            )

        CellClicked row col ->
            withParsedGrid
                (\grid ->
                    ( { grid | editedCell = Just ( row, col ) }
                    , Task.attempt (always NoOp) (Dom.focus (toCellId row col))
                    )
                )
                model

        CellLostFocus ->
            withParsedGridPure (\grid -> { grid | editedCell = Nothing }) model

        CellTyped row col str ->
            withParsedGridPure (\grid -> { grid | cells = Dict.insert ( row, col ) str grid.cells }) model

        ChangeRowCount delta ->
            withParsedGridPure (\grid -> { grid | rowCount = grid.rowCount + delta }) model

        ChangeColumnCount delta ->
            withParsedGridPure (\grid -> { grid | columnCount = grid.columnCount + delta }) model

        SwapColumns col1 col2 ->
            withParsedGridPure (\grid -> { grid | cells = swapColumns col1 col2 grid.cells }) model

        NoOp ->
            ( model, Cmd.none )


withParsedGrid : (Grid -> ( Grid, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
withParsedGrid f model =
    case model.parsedGrid of
        Ok grid ->
            let
                ( newGrid, cmd ) =
                    f grid
            in
            ( { model | parsedGrid = Ok newGrid }, cmd )

        Err _ ->
            ( model, Cmd.none )


withParsedGridPure : (Grid -> Grid) -> Model -> ( Model, Cmd Msg )
withParsedGridPure f model =
    ( case model.parsedGrid of
        Ok grid ->
            { model | parsedGrid = Ok <| f grid }

        Err _ ->
            model
    , Cmd.none
    )


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
    List.indexedMap (\rowIdx record -> List.indexedMap (\colIdx cellValue -> ( ( rowIdx, colIdx ), cellValue )) record) records
        |> List.concat
        |> Dict.fromList


gridToRecords : Grid -> Records
gridToRecords { rowCount, columnCount, cells } =
    List.range 0 (rowCount - 1)
        |> List.map
            (\rowIdx ->
                List.range 0 (columnCount - 1)
                    |> List.map (\colIdx -> Dict.get ( rowIdx, colIdx ) cells |> Maybe.withDefault "")
            )


gridToCsv : Grid -> String
gridToCsv =
    gridToRecords >> List.map (String.join ",") >> String.join "\n"


blobToRecords : String -> Records
blobToRecords str =
    let
        rows =
            String.split "\n" str
    in
    List.map (String.split ",") rows


parseGrid : String -> Result ParseError Grid
parseGrid csv =
    let
        records =
            blobToRecords csv

        allLinesHaveSameNumberOfColumns =
            List.length (List.unique <| List.map List.length records) == 1
    in
    case records of
        [] ->
            Err NotEnoughRows

        firstRow :: _ ->
            if String.isEmpty csv then
                Err NotEnoughRows

            else if not allLinesHaveSameNumberOfColumns then
                Err <| NumberOfColumnsNotTheSame <| List.map List.length records

            else
                Ok
                    { cells = recordsToCells records
                    , rowCount = List.length records
                    , columnCount = List.length firstRow
                    , editedCell = Nothing
                    }



--- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "CSV Data" ]
        , Html.textarea
            [ Events.onInput CsvInputChanged
            , A.rows 5
            , A.cols 40
            , A.value <|
                case model.parsedGrid of
                    Ok grid ->
                        gridToCsv grid

                    Err _ ->
                        model.csv
            , A.placeholder "Paste CSV data here ..."
            ]
            []
        , case model.parsedGrid of
            Ok grid ->
                viewGrid grid

            Err e ->
                viewError e
        ]


viewError : ParseError -> Html a
viewError parseError =
    Html.text <|
        case parseError of
            NotEnoughRows ->
                "You need to enter at least one row"

            NumberOfColumnsNotTheSame ints ->
                "All rows should have the same number of columns, but your rows have the following number of columns: "
                    ++ (String.join "," <| List.map String.fromInt ints)


viewGrid : Grid -> Html Msg
viewGrid grid =
    let
        renderTable =
            Html.table [] <|
                renderHeader
                    :: List.map renderRow (List.range 0 (grid.rowCount - 1))

        renderHeader =
            Html.tr [] <| List.map renderHeaderCell <| List.range 0 (grid.columnCount - 1)

        renderHeaderCell col =
            Html.th []
                ((if col > 0 then
                    [ Html.span [ Events.onClick <| SwapColumns col (col - 1) ] [ Html.text "◀ " ] ]

                  else
                    []
                 )
                    ++ (if col < grid.columnCount - 1 then
                            [ Html.span [ Events.onClick <| SwapColumns col (col + 1) ] [ Html.text " ▶" ] ]

                        else
                            []
                       )
                )

        renderRow row =
            Html.tr [] <| List.map (renderCell row) <| List.range 0 (grid.columnCount - 1)

        renderCell row col =
            Html.td
                [ Events.onClick (CellClicked row col)
                , A.style "width" <| (String.fromInt <| 100 // grid.columnCount) ++ "%"
                ]
                [ renderValue row col ]

        renderValue row col =
            let
                valString =
                    getValueAt row col grid.cells
            in
            if grid.editedCell == Just ( row, col ) then
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
            gridToRecords grid

        myTree =
            TreeBuilder.buildTree "<root>" records
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Parsed Data" ]
        , tableControls grid.rowCount grid.columnCount
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
