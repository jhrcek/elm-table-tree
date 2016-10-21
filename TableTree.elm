module TableTree exposing (..)

import Html exposing (text)
import Html.Attributes as A
import Html.App
import Html.Events exposing (onInput, targetValue, onClick, onBlur)
import Debug exposing (log)
import String
import Dict exposing (Dict)


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


type alias Model =
    { cells : Cells
    , rowCount : Int
    , columnCount : Int
    , blob : List (List String)
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
    { cells =
        Dict.fromList
            [ ( 0, 0 ) => "one"
            , ( 1, 0 ) => "two"
            , ( 2, 0 ) => "three"
            , ( 3, 0 ) => "four"
            , ( 0, 1 ) => "10"
            , ( 1, 1 ) => "20"
            , ( 2, 1 ) => "30"
            , ( 3, 1 ) => "40"
            , ( 0, 2 ) => "foo"
            , ( 1, 2 ) => "bar"
            , ( 2, 2 ) => "baz"
            , ( 3, 2 ) => "quux"
            ]
    , rowCount = 4
    , columnCount = 3
    , blob = []
    , editedCell = Nothing
    }


getValueAt : Int -> Int -> Cells -> String
getValueAt row col cells =
    Dict.get ( row, col ) cells |> Maybe.withDefault ""



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case Debug.log "" msg of
        NoOp ->
            model

        BlobPasted str ->
            { model | blob = parseBlob str }

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


parseBlob : String -> List (List String)
parseBlob str =
    let
        rows =
            String.split "\n" str
    in
        List.map (String.split ",") rows


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

        controls =
            Html.div []
                [ Html.button [ onClick (ChangeRowCount 1) ] [ text "Add row" ]
                , Html.button [ onClick (ChangeRowCount -1) ] [ text "Remove row" ]
                , Html.button [ onClick (ChangeColumnCount 1) ] [ text "Add  column" ]
                , Html.button [ onClick (ChangeColumnCount -1) ] [ text "Remove column" ]
                ]
    in
        Html.div []
            [ renderTable
            , controls
            , Html.textarea [ onInput BlobPasted, A.rows 5, A.cols 40 ] []
            , Html.hr [] []
            , Html.text <| toString model
            ]



-- HELPERS


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )
