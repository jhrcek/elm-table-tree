module Main exposing (..)

import Html exposing (text)
import Html.Attributes as A
import Html.App
import Html.Events exposing (onInput, targetValue, onClick, onBlur)
import Debug
import String
import Dict exposing (Dict)
import TreeBuilder


main : Program Never
main =
    Html.App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }



-- MODEL


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
    = NoOp
    | CellTyped Int Int String
    | CellClicked Int Int
    | CellUnClicked Int Int
    | BlobPasted String
    | ChangeRowCount Int
    | ChangeColumnCount Int
    | SwapColumns Int Int


initialModel : Model
initialModel =
    { cells = blobToCells initialBlob
    , rowCount = 4
    , columnCount = 4
    , editedCell = Nothing
    }


initialBlob : String
initialBlob =
    "Jan,33,Tester,male\nJane,40,Shop assistant,female\nDan,60,Professor,male\nLydia,40,Clerk,female"


getValueAt : Int -> Int -> Cells -> String
getValueAt row col cells =
    Dict.get ( row, col ) cells |> Maybe.withDefault ""



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case Debug.log "" msg of
        NoOp ->
            model

        BlobPasted blob ->
            { model | cells = blobToCells blob }

        CellClicked row col ->
            { model | editedCell = Just ( row, col ) }

        CellUnClicked row col ->
            { model
                | cells = Dict.update ( row, col ) updateCellValue model.cells
                , editedCell = Nothing
            }

        CellTyped row col str ->
            { model
                | cells = Dict.insert ( row, col ) str model.cells
            }

        ChangeRowCount delta ->
            { model | rowCount = clamp 0 10 <| model.rowCount + delta }

        ChangeColumnCount delta ->
            { model | columnCount = clamp 0 10 <| model.columnCount + delta }

        SwapColumns col1 col2 ->
            { model | cells = swapColumns col1 col2 model.cells }


updateCellValue : Maybe String -> Maybe String
updateCellValue mayCell =
    case mayCell of
        Nothing ->
            Nothing

        Just "" ->
            Nothing

        Just s ->
            Just s


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
                [0..columnCount - 1]
        )
        [0..rowCount - 1]


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


view : Model -> Html.Html Msg
view model =
    let
        renderTable =
            Html.table [] <|
                [ renderHeader ]
                    ++ (List.map renderRow [0..model.rowCount - 1])

        renderHeader =
            Html.tr [] (List.map renderHeaderCell [0..model.columnCount - 1])

        renderHeaderCell col =
            Html.th []
                ((if col > 0 then
                    [ Html.span [ onClick (SwapColumns col (col - 1)) ] [ text "◀ " ] ]
                  else
                    []
                 )
                    ++ (if col <= (model.columnCount - 2) then
                            [ Html.span [ onClick (SwapColumns col (col + 1)) ] [ text " ▶" ] ]
                        else
                            []
                       )
                )

        renderRow row =
            Html.tr [] (List.map (renderCell row) [0..model.columnCount - 1])

        renderCell row col =
            Html.td
                [ onClick (CellClicked row col)
                , A.style [ ( "width", (toString <| 100 // model.columnCount) ++ "%" ) ]
                ]
                [ renderValue row col ]

        renderValue row col =
            let
                valString =
                    getValueAt row col model.cells
            in
                if model.editedCell == Just ( row, col ) then
                    Html.input
                        [ A.value valString
                        , onInput (CellTyped row col)
                        , onBlur (CellUnClicked row col)
                        , A.style [ ( "display", "table-cell" ), ( "width", "99%" ) ]
                        ]
                        []
                else
                    Html.text valString

        tableControls =
            Html.div []
                [ Html.button [ onClick (ChangeRowCount 1) ] [ text "Add row" ]
                , Html.button [ onClick (ChangeRowCount -1) ] [ text "Remove row" ]
                , Html.button [ onClick (ChangeColumnCount 1) ] [ text "Add  column" ]
                , Html.button [ onClick (ChangeColumnCount -1) ] [ text "Remove column" ]
                ]

        records =
            cellsToRecords model.rowCount model.columnCount model.cells
    in
        Html.div []
            [ Html.h2 [] [ Html.text "CSV Data" ]
            , Html.textarea [ onInput BlobPasted, A.rows model.rowCount, A.cols 40, A.value (recordsToBlob records), A.placeholder "Paste CSV data here ..." ] []
            , Html.h2 [] [ Html.text "Parsed Data" ]
            , tableControls
            , renderTable
            , Html.h2 [] [ Html.text "Tree View" ]
            , TreeBuilder.drawTree <| TreeBuilder.buildTree "<root>" records
            , Html.hr [] []
            , Html.text <| toString model
            ]
