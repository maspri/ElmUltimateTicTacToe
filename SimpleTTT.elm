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
    { field : Field, drawingSize : Int, state : GameState, pos : Int,numFree:Int }



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
winningCombinations = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,6],[2,5,8],[0,4,8],[6,4,2]]

won : Player -> Field -> Bool
won player field = List.any (List.all (\i -> case (get i field) of
                                    Just (Pl pl) -> player == pl
                                    _ -> False )) winningCombinations

whowon : Int -> Field -> GameState
whowon numFree field = case (won PlayerX field,won PlayerO field) of
                (True,False) -> Won PlayerX
                (False,True) -> Won PlayerO
                (False,False) -> if numFree == 0 then Stalemate else Ongoing
                _ -> Debug.crash "should not happen"


update : Action -> Model -> Model
update a model =
    case a of
        SelectField x player ->
            case (get x model.field) of
                Just Free -> let newField =  set x (Pl player) model.field
                             in
                             { model | field = newField , state = whowon (model.numFree-1) newField, numFree = model.numFree-1}


                _ ->  model

        Reset ->
            initialModel model.pos



-- VIEW


view : Signal.Address UAction -> Bool -> Model -> Element
view address highlight { field, drawingSize, state, pos } =
    flow
        outward
        [ if highlight then image 300 300 "images/high_field.png" else image 300 300 "images/field.png"
        , flow down <| List.map (\i -> flow right <| toList <| indexedMap (\j c -> getImage address ( pos, i * 3 + j ) c) (slice (i * 3) (i * 3 + 3) field)) [0..2]
          --, flow down <| toList <| indexedMap (\i row -> flow right (toList <| indexedMap (\j elem -> getImage address ( i, j ) elem) row)) field
        ]


getImage : Signal.Address UAction -> ( Int, Int ) -> Content -> Element
getImage address pos c =
    clickable (message address (Play pos))
        <| case c of
            Free ->
                spacer 100 100

            Pl PlayerX ->
                image 100 100 "images/cross.png"

            Pl PlayerO ->
                image 100 100 "images/circle.png"


initialModel : Int -> Model
initialModel p =
    { field = repeat 9 Free, drawingSize = 300, state = Ongoing, pos = p ,numFree = 9}
