module Playground exposing
    ( doubleScores
    , highestScores
    , main
    , scoresLessThan320
    )

import Html

escapeEarth myVelocity mySpeed =
    if myVelocity > 11.186 then
        "Godspeed"

    else if mySpeed == 7.67 then
        "Stay in orbit"

    else
        "Come back"


revelation =
    """
    It became very clear to me sitting out there today
    that every decision I've made in my entire life has
    been wrong. My life is the complete "opposite" of
    everything I want it to be. Every instinct I have,
    in every aspect of life, be it something to wear,
    something to eat - it's all been wrong.
    """

outerMultiplier =
    6

multiplyByFive number =
    let
        multiplier =
            8
    in
    number * multiplier

scoresLessThan320 scores =
    List.filter isLessThan320 scores


isLessThan320 score =
    score < 320

scoreMultiplier =
    2


highestScores =
    [ 316, 320, 312, 370, 337, 318, 314 ]


doubleScores scores =
    List.map (\x -> x * scoreMultiplier) scores
main =
    multiplyByFive 3
            |> Debug.toString
            |> Html.text
