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
    }


view : Computer -> Memory -> List Shape
view computer memory =
    [ (List.repeat 100 (cube grey grey lightGrey 100)
        |> List.indexedMap (\idx -> move3d (toFloat idx * 150) -300 0)
      )
        ++ (List.repeat 100 (cube grey lightGrey grey 100)
                |> List.indexedMap (\idx -> move3d (toFloat idx * 150) 300 0)
           )
        ++ [ cube lightRed red red 50
                |> rotate3d 0 memory.r 0
                |> move3d memory.x memory.y memory.z
           ]
        |> group3d
        |> shape3dto2d (camera ( memory.x + 400, 0, -200 ) ( memory.x - 1000, 0, 0 ))
    ]


update : Computer -> Memory -> Memory
update computer memory =
    (if computer.mouse.click == True then
        { memory
            | zmove = -15
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
                        | zmove = 0
                    }

                else
                    { mem
                        | zmove = mem.zmove + 1
                    }
           )


init : Memory
init =
    { x = 17000
    , y = 0
    , z = 0
    , r = 0
    , zmove = 0
    }
