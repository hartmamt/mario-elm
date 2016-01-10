import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }

type alias Platform =
  { x : Float
  , y : Float
  , w: Float
  , vx : Float
  , vy : Float
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }

cloud : Platform
cloud =
  { x = 100
  , y = 400
  , w = 50
  , vx = 0
  , vy = 0
  }


-- UPDATE

update : (Float, Keys, Bool, Bool) -> Model -> Model
update (dt, keys, isRunning, isMoonWalking) mario =
  mario
    |> gravity (dt, cloud)
    |> jump keys
    |> fall keys
    |> walk (isRunning, isMoonWalking, keys)
    |> physics (dt, cloud)


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 -- && mario.vy == 0
    then { mario | vy = 6.0 }
    else mario

fall : Keys -> Model -> Model
fall keys mario =
  if keys.y < 0
    then { mario | vy = -6.0 }
    else mario

gravity : (Float, Platform) -> Model -> Model
gravity (dt, cloud) mario =
  { mario |
      vy =
        if mario.y > 0 then
          mario.vy - dt/5 else 0
  }


physics : (Float, Platform) -> Model -> Model
physics (dt, cloud) mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y =
        if (mario.y >= cloud.y && (mario.x >= cloud.x && mario.x <= cloud.x + cloud.w)) then
          max cloud.y (mario.y + dt * mario.vy)
        else max 0 (mario.y + dt * mario.vy)
  }

walk : (Bool, Bool, Keys) -> Model -> Model
walk (isRunning, isMoonWalking, keys) mario =
  { mario |
      vx =
        if isRunning then
          if isMoonWalking then toFloat keys.x * -4
          else toFloat keys.x * 4
        else if isMoonWalking then toFloat keys.x * -1
        else toFloat keys.x * 1,
      dir =
        if keys.x < 0 then Left
        else if keys.x > 0 then Right
        else mario.dir

  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if  mario.y  >  0 then "jump"
      else if mario.vx /= 0 then "walk"
      else "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "/imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 32 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , rect 100 50
          |> filled (rgb 255 255 255)
          |> move (100, 400 - h/2)
      , marioImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal (Float, Keys, Bool, Bool)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map4 (,,,) delta Keyboard.arrows Keyboard.shift Keyboard.space)
