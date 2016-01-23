module UltimateTTT (..) where

import SimpleTTT exposing (Player,Mode)
import Array exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Signal exposing (..)
import Text exposing (..)
import Color exposing (..)


-- MODEL




type GameState
    = Won Player
    | Stalemate
    | Ongoing
    | NotStarted


type alias Model =
    { board : Array SimpleTTT.Model,
     drawingSize : Int,
     state : GameState,
     mode : Mode,
     currentPlayer : Player,
     lastPlay : Maybe Int}



-- UPDATE


type alias Action =
    SimpleTTT.UAction


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        SimpleTTT.PlayerX ->
            SimpleTTT.PlayerO

        SimpleTTT.PlayerO ->
            SimpleTTT.PlayerX



won : Player -> Model -> Bool
won player model = List.any (List.all (\i -> case (get i model.board) of
                                    Just simpleModel -> case simpleModel.state of
                                      SimpleTTT.Won pl-> pl == player
                                      SimpleTTT.Ongoing -> False
                                      SimpleTTT.Stalemate -> False
                                    _ -> False )) SimpleTTT.winningCombinations

{--whowon : Model -> GameState
whowon model = case (won SimpleTTT.PlayerX model,won SimpleTTT.PlayerO model) of
                (True,False) -> Won SimpleTTT.PlayerX
                (False,True) -> Won SimpleTTT.PlayerO
                (False,False) -> if (List.any (\subfield -> subfield.state == SimpleTTT.Ongoing) (toList model.board)) then Ongoing else Stalemate
                _ -> Debug.crash "should not happen"--}

won2 : Player -> Model -> GameState
won2  p model = if  Debug.watch " player won: " (won p model) then Won p
                         else (if (List.any (\subfield -> subfield.state == SimpleTTT.Ongoing) (toList model.board)) then Ongoing else Stalemate)



update : Action -> Model -> Model
update a model =
    case a of
        SimpleTTT.Play ( x, y ) ->
            case (get x model.board) of
                Just field ->
                    let
                        valid = (case (get y field.field) of
                                    Just (SimpleTTT.Free) -> True
                                    _ -> False) &&
                                 (field.state == SimpleTTT.Ongoing) &&
                                 (case model.lastPlay of
                                    Just i -> i==x
                                    Nothing  -> True) && (model.state == Ongoing)


                        newBoard = set x (SimpleTTT.update (SimpleTTT.SelectField y model.currentPlayer) field) model.board

                        newLastPlay = case (get y model.board) of
                          Just subfield -> case subfield.state of
                            SimpleTTT.Ongoing -> Just y
                            SimpleTTT.Stalemate -> Nothing
                            SimpleTTT.Won _ -> Nothing
                          _ -> Debug.crash "should not happen3"

                        newModel = { model | board = newBoard, currentPlayer = otherPlayer model.currentPlayer, lastPlay = newLastPlay ,state=(won2 model.currentPlayer model)}
                    in
                      if Debug.watch "valid" valid then
                          { newModel | state=Debug.watch "GameState" (won2 model.currentPlayer model)}
                      else model

                Nothing ->
                    Debug.crash "cannot happen"

        SimpleTTT.UReset ->
            init
        SimpleTTT.SelectStart -> {model | state = Ongoing}
        SimpleTTT.Select newmode -> {model | mode = newmode }


fieldClickedMailbox : Mailbox Action
fieldClickedMailbox =
    Signal.mailbox SimpleTTT.UReset


-- VIEW

init : Model
init =
    { board = (initialize 9 SimpleTTT.initialModel), drawingSize = 300, state = NotStarted, mode = SimpleTTT.PvP, currentPlayer = SimpleTTT.PlayerX, lastPlay = Nothing }


view : Signal.Address SimpleTTT.UAction -> Model -> Element
view address model =
  case model.state of
      Ongoing -> drawModel address model
      Won p -> flow down [drawModel address model,flow right [show "Player: ",show p,show " won."]]
      Stalemate -> flow down [drawModel address model,show "Stalemate"]
      NotStarted -> startView address


startView: Signal.Address SimpleTTT.UAction -> Element
startView address = container 300 300 middle <|
                    flow down [clickable (message address SimpleTTT.SelectStart) <| centered <| Text.color red <| Text.height 80 (fromString "Start"),
                       width 180 <| dropDown (\mode -> message address <| SimpleTTT.Select mode) [("Player vs. Player",SimpleTTT.PvP),("Player vs AI",SimpleTTT.PvAI)]
                      ]

drawModel : Signal.Address SimpleTTT.UAction -> Model -> Element
drawModel address model = flow outward
                                [ image 900 900 "images/field.jpg"
                                , flow down <| List.map (\i -> flow right <| toList <| indexedMap (\j c -> SimpleTTT.view address (model.lastPlay == Just (i*3+j)) c) (slice (i * 3) (i * 3 + 3) model.board)) [0..2]
                                  --flow down <| toList <| indexedMap (\i row -> flow right (toList <| indexedMap (\j elem -> SimpleTTT.view address elem) row)) board
                                ]




main : Signal Element
main =
    Signal.map (view fieldClickedMailbox.address) (Signal.foldp update init fieldClickedMailbox.signal)
