port module NumberRun exposing (..)

import Browser
import Html as H exposing (Html, div, button, ul, li, span, text, p)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Random
import Set exposing (Set)
import Tuple exposing (pair, first, second)

calculate_distances = False

port calculateDistance : (Int,Int) -> Cmd msg
port receiveDistance : (Int -> msg) -> Sub msg

type Screen
    = IntroScreen
    | GameScreen

type alias GameModel = 
    { current : Int
    , target : Int
    , history: List Int
    , primes : List Int
    , min_distance : Maybe Int
    }

type alias Model =
    { game : GameModel
    , screen : Screen
    }

type Msg
    = GoToGame
    | GameMsg GameMsg

type GameMsg
    = MoveTo Int
    | Start Int Int
    | StartAgain
    | GoToIntro
    | Undo
    | ReceiveDistance Int

main = Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> receiveDistance (ReceiveDistance >> GameMsg)
    , view = view
    }

sups = String.toList "⁰¹²³⁴⁵⁶⁷⁸⁹"
digits = String.toList "0123456789"
superscript n =
    let
        tosup d = (Maybe.withDefault d << Maybe.map first << List.head << List.filter (second >> (==) d)) (List.map2 pair sups digits)
    in
        (String.fromList << List.map tosup << String.toList << fi) n

is_prime : List Int -> Int -> (Bool, List Int)
is_prime eprimes n =
    let
        primes = primes_upto n eprimes
    in
        (not <| List.any (\p -> p /= n && modBy p n == 0) primes, primes)

primes_upto : Int -> List Int -> List Int
primes_upto n eprimes =
    let
        m = Maybe.withDefault 3 (List.maximum eprimes)
        step i ps = 
            if i >n then 
                ps 
            else
                let
                    nps = if List.any (\p -> modBy p i == 0) ps then ps else i::ps
                in
                    step (i+2) nps
    in
        if m >= n then
            eprimes
        else
            step (m+2) eprimes

pick_target : Cmd GameMsg
pick_target = 
    let
        rcurrent = Random.int 1000 20000
        rtarget = Random.int 1000 20000
        msg = Random.map2 Start rtarget rcurrent
    in
        Random.generate identity msg


init : () -> (Model, Cmd Msg)
init _ = 
    ( { game =
          { current = 1000 
          , target = 1
          , history = []
          , primes = [3,2]
          , min_distance = Nothing
          }
      , screen = IntroScreen
      }
    
    , Cmd.map GameMsg pick_target
    )

remove_factor a x =
    if modBy a x == 0 then
        remove_factor a (x//a)
    else
        x

prime_factors x =
    let
        step : Int -> (Int, List Int) -> (Int, List Int)
        step p (n,factors) = if modBy p n == 0 then (remove_factor p n, p::factors) else (n,factors)
    in
        (List.reverse << second) <| List.foldl step (x,[]) (List.range 2 x)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GoToGame -> ({ model | screen = GameScreen}, Cmd.none)
    GameMsg m -> 
        case m of
            GoToIntro -> ({ model | screen = IntroScreen}, Cmd.none)
            _ -> 
                let
                    (game, cmd) = update_game m model.game
                in
                    ({ model | game = game}, Cmd.map GameMsg cmd)

update_game : GameMsg -> GameModel -> (GameModel, Cmd GameMsg)
update_game msg model = case msg of
    Start target current -> 
        let
            (tprime, p1) = is_prime model.primes target
            (cprime, p2) = is_prime p1 current
            cmd = 
                if tprime || current==target then 
                    pick_target 
                else if calculate_distances then 
                    (calculateDistance (current,target)) 
                else 
                    Cmd.none
        in
            ({ model | target = target, current = current, history = [], min_distance = Nothing, primes = p2 }, cmd)
    MoveTo i -> 
        ( if i == model.current then model else { model | current = i, history=model.current::model.history }
        , Cmd.none
        )
    StartAgain -> (model, pick_target)
    GoToIntro -> (model, Cmd.none)
    Undo -> case List.head model.history of
        Just x -> ({ model | current = x, history = List.drop 1 model.history }, Cmd.none)
        Nothing -> (model, Cmd.none)
    ReceiveDistance d -> 
        let
            q = Debug.log "receive distance" d
        in
            ({ model | min_distance = Just d }, Cmd.none)

fi = String.fromInt
strf : String -> List String -> String
strf template bits =
    let
        next_bit cbits = case cbits of
            a::rest -> (a,rest)
            [] -> ("",[])
    in
        Tuple.first <| List.foldl (\chr -> \(out,cbits) -> 
            if chr=='%' then
                let
                    (suffix,nbits) = next_bit cbits
                in
                    (out++suffix, nbits)
            else
                (out++(String.fromChar chr), cbits)
        ) ("",bits) (String.toList template)

view : Model -> Html Msg
view model = case model.screen of
    IntroScreen -> view_intro model
    GameScreen -> H.map GameMsg (view_game model.game)

view_intro model =
    div
        [ HA.id "intro" ]
        [ div
            [ HA.id "rules" ]
            (List.map (\t -> p [] [ text t ])
                [ "You start at a random number, with a random target."
                , "Try to reach the target, by adding or removing any prime factor of your current number."
                ]
            )
        , button
            [ HA.id "start-game" 
            , HE.onClick GoToGame
            ]
            [ text "Start the game" ]
        ]


view_game : GameModel -> Html GameMsg
view_game model =
    div
        [ HA.id "game" ]
        [ div
            [ HA.id "controls" ]
            [ button 
                [ HA.id "to-intro"
                , HE.onClick GoToIntro
                ]
                [ text "How to play" ]
            , button 
                [ HA.id "start-again"
                , HE.onClick StartAgain
                ]
                [ text "Start again" ]
            ]
        , view_target model
        , view_steps model
        , div
            [ HA.id "options" ]
            (if model.target /= model.current then
                [ H.h2 [] [ text "Where next?" ]
                , view_options model
                ]
             else
                [ HK.ul
                    [ HA.id "factors" ]
                    [("undo", button [ HE.onClick Undo, HA.id "undo", HA.disabled (model.history == [])] [text "Undo" ])]
                ]
            )
        ]

view_steps model =
    let
        num_steps = List.length model.history
        step_count = strf "% %" [ fi num_steps, if num_steps == 1 then "step" else "steps"]
        line = 
            if model.target == model.current then 
                strf "You did it in %!" [step_count]
            else 
                strf "% so far." [step_count]
        partext = 
            let
                dtext = case model.min_distance of 
                    Nothing -> "???"
                    Just d -> fi d
            in
                strf " (par: %)" [dtext]
    in
        p
            [ HA.id "steps" 
            , HA.classList
                [ ("success", model.target == model.current)
                ]
            ]
            [ text <| line++(if calculate_distances then partext else "") ]

show_prime_factorisation : List Int -> Int -> String
show_prime_factorisation eprimes n =
    let
        sqrn = (floor << sqrt << toFloat) n
        primes = List.filter ((>=) n) (primes_upto sqrn eprimes)
        pow i j p = if modBy p j == 0 then pow (i+1) (j//p) p else i
        powers = (List.filter (second >> (/=) 0) << List.map (\p -> (p, pow 0 n p))) primes
    in
        String.join " × " (List.map (\(p,e) -> (fi p)++(if e==1 then "" else superscript e)) powers)

view_target model =
    div
        [ HA.class "state" ]
        ( [ div [ HA.class "text target" ] [ text "Target" ]
          , div [ HA.class "target" ] [ text <| strf "% = %" [show_prime_factorisation model.primes model.target, fi model.target] ]
 --         , div [ HA.class "target" ] [ text <| " = "++(show_prime_factorisation model.primes model.target) ]
          , div [ HA.class "text history current" ] [ text "Currently" ]
          , div [ HA.class "history current", HA.attribute "aria-live" "assertive" ] [ text <| strf "% = %" [show_prime_factorisation model.primes model.current, fi model.current] ]
          ]
        ++(view_history model)
        )

view_history model =
    let
        item n (col,last,out) =
            ( col+1,n
            , (div 
                [ HA.class "history" ] 
                [ text <| fi n ]
              )
              ::
              (div
                [ HA.class "change" 
                , HA.style "grid-column" (fi col)
                ] 
                [ text <| if n<last then "+" ++ (fi (last-n)) else "-" ++ (fi (n-last)) ]
              )
              ::
              out
            )
    in
        List.reverse << (\(_,_,l) -> l) <| List.foldl item (3,model.current,[]) model.history

view_options model = 
    let
        factors = prime_factors model.current
        top_factor = (Maybe.withDefault 2 << List.maximum) factors
        prime_range = List.filter ((>=) top_factor) model.primes
    in
        HK.ul
            [ HA.id "factors" ]
            (
                [("undo", button [ HE.onClick Undo, HA.id "undo", HA.disabled (model.history == [])] [text "Undo" ])]
                ++
                (List.map 
                (\p -> 
                    if List.member p factors then
                        (fi p, factor_options model.target model.current p)
                    else
                        (fi p, div [] [])
                )
                (List.reverse prime_range)
                )
            )

factor_options target current f =
    let
        start = ((max 0 (current - f)) - current)//f
        end = 1
    in
        li
            [ HA.class "factor"
            , HA.attribute "data-factor" (fi f)
            ]
            [ div [] [ text <| strf "% × %" [fi f, fi (current//f)] ]
            , ul
                []
                (List.map (\c -> view_option target (current + c*f)) (List.filter ((/=) 0) (List.range start end)))
            ]

view_option target x =
    li
        [ HA.classList
            [ ("less", x<target)
            , ("more", x>target)
            , ("equal", x==target)
            ]
        ]
        [ button
            [ HE.onClick (MoveTo x)
            , HA.disabled (x==0)
            ]
            [ text <| String.fromInt x ]
        ]
