module SimpleTTT (..) where

import Array exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Signal exposing (..)


-- MODEL


type Player
    = PlayerX
    | PlayerO


type Content
    = Pl Player
    | Free


type alias Field =
    Array Content


type GameState
    = Won Player
    | Stalemate
    | Ongoing


type alias Model =
    { field : Field, drawingSize : Int, state : GameState, pos : Int, numFree : Int }



-- UPDATE


type Mode
    = PvP
    | PvAI


type UAction
    = Play ( Int, Int )
    | UReset
    | SelectStart
    | Select Mode


type Action
    = SelectField Int Player
    | Reset


winningCombinations : List (List Int)
winningCombinations =
    [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ], [ 0, 4, 8 ], [ 6, 4, 2 ] ]


won : Player -> Field -> Bool
won player field =
    List.any
        (List.all
            (\i ->
                case (get i field) of
                    Just (Pl pl) ->
                        player == pl

                    _ ->
                        False
            )
        )
        winningCombinations


whowon : Int -> Field -> GameState
whowon numFree field =
    case ( won PlayerX field, won PlayerO field ) of
        ( True, False ) ->
            Won PlayerX

        ( False, True ) ->
            Won PlayerO

        ( False, False ) ->
            if numFree == 0 then
                Stalemate
            else
                Ongoing

        _ ->
            Debug.crash "should not happen"


update : Action -> Model -> Model
update a ({ field, numFree, drawingSize, pos } as model) =
    case a of
        SelectField x player ->
            case (get x field) of
                Just Free ->
                    let
                        newField = set x (Pl player) field
                    in
                        { model | field = newField, state = whowon (numFree - 1) newField, numFree = numFree - 1 }

                _ ->
                    model

        Reset ->
            initialModel drawingSize pos



-- VIEW


view : Signal.Address UAction -> Bool -> Model -> Element
view address highlight { field, drawingSize, state, pos } =
    case state of
        Ongoing ->
            flow
                outward
                [ if highlight then
                    image drawingSize drawingSize "images/high_field.png"
                  else
                    image drawingSize drawingSize "images/field.png"
                , flow down <| List.map (\i -> flow right <| toList <| indexedMap (\j c -> getImage address (drawingSize // 3) ( pos, i * 3 + j ) c) (slice (i * 3) (i * 3 + 3) field)) [0..2]
                ]

        Won PlayerX ->
            image drawingSize drawingSize "images/cross.png"

        Won PlayerO ->
            image drawingSize drawingSize "images/circle.png"

        Stalemate ->
            image drawingSize drawingSize "images/stalemate.png"


getImage : Signal.Address UAction -> Int -> ( Int, Int ) -> Content -> Element
getImage address drawsize pos c =
    clickable (message address (Play pos))
        <| case c of
            Free ->
                spacer drawsize drawsize

            Pl PlayerX ->
                image drawsize drawsize "images/cross.png"

            Pl PlayerO ->
                image drawsize drawsize "images/circle.png"


initialModel : Int -> Int -> Model
initialModel drawsize p =
    { field = repeat 9 Free, drawingSize = drawsize, state = Ongoing, pos = p, numFree = 9 }
