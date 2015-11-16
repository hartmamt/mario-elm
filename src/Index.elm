import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { mario : Mario
  , ball : Ball
  , gameOver: Bool
  }


type alias Mario =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }

type alias Ball =
  { x : Float
  , r : Float
  , vx : Float
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Mario
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


ballMaxDistance : Float
ballMaxDistance =
  200


ball : Ball
ball =
  { x = -1*ballMaxDistance
  , r = 10
  , vx = 1
  }


model : Model
model =
  { mario = mario
  , ball = ball
  , gameOver = False
  }

-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
  let
    newMario =
      model.mario
        |> gravity dt
        |> jump keys
        |> walk keys
        |> physics dt
  in
    { model | mario <- newMario
    }


jump : Keys -> Mario -> Mario
jump keys mario =
  if keys.y > 0 && mario.vy == 0
    then { mario | vy <- 6.0 }
    else mario


gravity : Float -> Mario -> Mario
gravity dt mario =
  { mario |
      vy <- if mario.y > 0 then mario.vy - dt/4 else 0
  }


physics : Float -> Mario -> Mario
physics dt mario =
  { mario |
      x <- mario.x + dt * mario.vx,
      y <- max 0 (mario.y + dt * mario.vy)
  }


walk : Keys -> Mario -> Mario
walk keys mario =
  { mario |
      vx <- toFloat keys.x,
      dir <-
        if  | keys.x < 0 -> Left
            | keys.x > 0 -> Right
            | otherwise  -> mario.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') model =
  let
    mario = model.mario

    (w,h) = (toFloat w', toFloat h')

    verb =
      if  | mario.y  >  0 -> "jump"
          | mario.vx /= 0 -> "walk"
          | otherwise     -> "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "/imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , marioImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update model input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
