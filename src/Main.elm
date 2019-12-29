module Main exposing (..)

import Playground exposing (..)
import Playground3d exposing (..)


main =
    game view update init


type alias Memory =
    { x : Number
    , y : Number
    , z : Number
    , r : Number
    , zmove : Number
    , jumpcount : Number
    , scene : Scene
    }


view : Computer -> Memory -> List Shape
view computer memory =
    [ List.concat
        [ []
        , [ polygon3d yellow
                [ ( 0, 0, 0 )
                , ( 0, 1000, 0 )
                , ( 0, 1000, 10 )
                , ( 0, 0, 10 )
                ]
                |> move3d 0 -500 0
          ]
        , List.repeat 30 (cube grey grey lightGrey 100)
            |> List.indexedMap (\idx -> move3d (toFloat idx * 300) -300 0)
        , List.repeat 30 (cube grey lightGrey grey 100)
            |> List.indexedMap (\idx -> move3d (toFloat idx * 300) 300 0)
        , [ cube lightGreen green green 50
                |> rotate3d 0 memory.r 0
                |> move3d memory.x memory.y memory.z
          ]
        ]
        |> group3d
        |> shape3dto2d (camera ( memory.x + 400, computer.mouse.x, memory.z - 200 - computer.mouse.y ) ( memory.x - 1000, 0, -200 ))
    , if memory.scene == Goal then
        words darkYellow "CLEAR!"
        |> scale 10

      else
        words white ""
    ]


update : Computer -> Memory -> Memory
update computer memory =
    if memory.scene == Goal then
        memory

    else
        (if computer.mouse.click == True && memory.jumpcount < 2 then
            { memory
                | zmove = -15
                , jumpcount = memory.jumpcount + 1
            }

         else
            memory
        )
            |> (\mem ->
                    { mem
                        | x = mem.x - 10
                        , r = mem.r + 5
                        , z = mem.z + mem.zmove
                    }
               )
            |> (\mem ->
                    if mem.z >= 0 then
                        { mem
                            | z = 0
                            , zmove = 0
                            , jumpcount = 0
                        }

                    else
                        { mem
                            | zmove = mem.zmove + 1
                        }
               )
            |> (\mem ->
                    if mem.x <= 0 then
                        { mem | scene = Goal }

                    else
                        mem
               )


init : Memory
init =
    { x = 10000
    , y = 0
    , z = 0
    , r = 0
    , zmove = 0
    , jumpcount = 0
    , scene = Running
    }


type Scene
    = Running
    | Goal
