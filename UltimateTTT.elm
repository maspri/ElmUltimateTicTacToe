module UltimateTTT (..) where

import SimpleTTT exposing (Player, Mode)
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
    { board : Array SimpleTTT.Model
    , drawingSize : Int
    , state : GameState
    , mode : Mode
    , currentPlayer : Player
    , lastPlay : Maybe Int
    }



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
won player model =
    List.any
        (List.all
            (\i ->
                case (get i model.board) of
                    Just simpleModel ->
                        case simpleModel.state of
                            SimpleTTT.Won pl ->
                                pl == player

                            SimpleTTT.Ongoing ->
                                False

                            SimpleTTT.Stalemate ->
                                False

                    _ ->
                        False
            )
        )
        SimpleTTT.winningCombinations


won2 : Player -> Model -> GameState
won2 p model =
    if Debug.watch " player won: " (won p model) then
        Won p
    else
        (if (List.any (\subfield -> subfield.state == SimpleTTT.Ongoing) (toList model.board)) then
            Ongoing
         else
            Stalemate
        )


update : Action -> Model -> Model
update a ({ board, drawingSize, state, mode, currentPlayer, lastPlay } as model) =
    case a of
        SimpleTTT.Play ( x, y ) ->
            case (get x board) of
                Just field ->
                    let
                        newField = (SimpleTTT.update (SimpleTTT.SelectField y currentPlayer) field)

                        newBoard = set x newField board

                        newLastPlay =
                            case (get y newBoard) of
                                Just subfield ->
                                    case subfield.state of
                                        SimpleTTT.Ongoing ->
                                            Just y

                                        SimpleTTT.Stalemate ->
                                            Nothing

                                        SimpleTTT.Won _ ->
                                            Nothing

                                _ ->
                                    Debug.crash "should not happen3"

                        newModel = { model | board = newBoard, currentPlayer = otherPlayer currentPlayer, lastPlay = newLastPlay }

                        newState = won2 currentPlayer newModel

                        valid =
                            (case (get y field.field) of
                                Just SimpleTTT.Free ->
                                    True

                                _ ->
                                    False
                            )
                                && (field.state == SimpleTTT.Ongoing)
                                && (Maybe.withDefault True (Maybe.map ((==) x) lastPlay))
                                && (state == Ongoing)
                    in
                        if Debug.watch "valid" valid then
                            { newModel | state = newState }
                        else
                            model

                Nothing ->
                    Debug.crash "cannot happen"

        SimpleTTT.UReset ->
            init drawingSize

        SimpleTTT.SelectStart ->
            { model | state = Ongoing }

        SimpleTTT.Select newmode ->
            { model | mode = newmode }


fieldClickedMailbox : Mailbox Action
fieldClickedMailbox =
    Signal.mailbox SimpleTTT.UReset



-- VIEW


init : Int -> Model
init drawSize =
    { board = (initialize 9 (SimpleTTT.initialModel (drawSize // 3))), drawingSize = drawSize, state = NotStarted, mode = SimpleTTT.PvP, currentPlayer = SimpleTTT.PlayerX, lastPlay = Nothing }


bigRedText : String -> Element
bigRedText s =
    leftAligned <| Text.color red <| Text.height 80 <| fromString s


view : Signal.Address SimpleTTT.UAction -> Model -> Element
view address ({ state, drawingSize } as model) =
    case state of
        Ongoing ->
            drawModel address model

        Won p ->
            flow outward [ drawModel address model, container drawingSize drawingSize middle <| bigRedText ((toString p) ++ " won.") ]

        Stalemate ->
            flow outward [ drawModel address model, container drawingSize drawingSize middle <| bigRedText "Stalemate" ]

        NotStarted ->
            startView address


startView : Signal.Address SimpleTTT.UAction -> Element
startView address =
    container 700 500 middle
        <| flow
            down
            [ Graphics.Element.size 250 80 <| button (message address SimpleTTT.SelectStart) "Start"
            , --clickable Graphics.Element.Element (message address SimpleTTT.SelectStart) <| centered <| Text.color red <| Text.height 120 (fromString "Start"),
              width 250 <| dropDown (\mode -> message address <| SimpleTTT.Select mode) [ ( "Player vs. Player", SimpleTTT.PvP ), ( "Player vs AI", SimpleTTT.PvAI ) ]
            ]


drawModel : Signal.Address SimpleTTT.UAction -> Model -> Element
drawModel address { drawingSize, board, lastPlay } =
    let
        highlighted =
            case lastPlay of
                Nothing ->
                    \_ _ -> True

                Just n ->
                    \x y -> n == x * 3 + y
    in
        flow
            outward
            [ image drawingSize drawingSize "images/field.png"
            , flow down
                <| List.map
                    (\i ->
                        flow right
                            <| toList
                            <| indexedMap (\j c -> SimpleTTT.view address (highlighted i j) c) (slice (i * 3) (i * 3 + 3) board)
                    )
                    [0..2]
            ]


main : Signal Element
main =
    Signal.map (view fieldClickedMailbox.address) (Signal.foldp update (init 720) fieldClickedMailbox.signal)
