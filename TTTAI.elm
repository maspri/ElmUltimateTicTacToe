module TTTAI (..) where

import SimpleTTT exposing (Player)
import UltimateTTT exposing (Model)
import Array exposing (..)
import Maybe exposing (andThen)



evaluate : Model-> Player -> Int
evaluate m p = 42

getMoves' : SimpleTTT.Model -> List Int
getMoves' {field,state} = snd (Array.foldl ( \content (i,xs)-> case content of
                                                                SimpleTTT.Free -> (i+1,i::xs)
                                                                _ -> (i+1,xs) ) (0,[]) field)

getMoves : UltimateTTT.Model -> Maybe List (Int,Int)
getMoves {board,state,currentPlayer,lastPlay} =
  lastPlay `andThen` (\i -> (get i board) `andThen` (\model -> Just <| List.map (\j -> (i,j)) (getMoves' model)))


--alphaBetaMax : Int -> Int -> Int -> Model -> ((Int,Int),Int)
--alphaBetaMax 0 _ _ model = evaluate model
--alphaBetaMax depth alpha beta = map (alphaBetaMin) (getMoves model)


--alphaBetaMin : Int -> Int -> Int -> Model -> ((Int,Int),Int)
--alphaBetaMin 0 alpha beta model =
--alphaBetaMin depth alpha beta =
