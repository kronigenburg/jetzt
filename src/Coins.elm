module Coins exposing
  ( Model, Msg(..), initModel, initCmd
  , view, update, subscriptions
  )


import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Browser

import Common.Camera as Camera exposing (Camera)
import Common.Events as Events
import Common.Fps as Fps
import Common.Meshes as Meshes exposing (Meshes)
import Common.Scene as Scene
import Common.Settings as Settings exposing
  ( Settings, SettingsMsg, settings )
import Cylinder3d
import Direction3d
import Duration
import Frame3d

import Camera3d exposing (Camera3d)
import Viewpoint3d

import Physics.Body as Body exposing (Body)
import Physics.Shape
import Physics.World as World exposing (World)
import Physics.Coordinates exposing
  ( BodyCoordinates, WorldCoordinates )
import Physics.Material

import Color
import Length exposing (Meters, meters, millimeters)
import Mass exposing (kilograms)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels, pixels)

import LuminousFlux
import Illuminance

import Scene3d
import Scene3d.Material
import Scene3d.Light
import Scene3d.Mesh

import Point3d
import Sphere3d
import Block3d
import Vector3d
import Acceleration
import Angle
import Axis3d

import Random
import Luminance exposing (Luminance)
import Luminance
import Html.Lazy


type alias Model=
  { world: World Data
  , width: Quantity Float Pixels
  , height: Quantity Float Pixels
  }

initModel: Model
initModel=
  { world= initialWorld
  , width= pixels 0
  , height= pixels 0
  }

camera: Camera3d Meters WorldCoordinates
camera=
  Camera3d.perspective
    { viewpoint=
        Viewpoint3d.lookAt
          { eyePoint= cameraPosition
          , focalPoint= Point3d.meters -0.5 2 2.5
          , upDirection= Direction3d.positiveZ
          }
    , verticalFieldOfView= Angle.degrees 32
    }

cameraPosition: Point3d.Point3d Meters coordinates
cameraPosition=
  Point3d.meters 6.5 14 6.8

type Msg=
  Tick Float
  | Resize Float Float
  | DropSome Int
  | AddRandom (Body Data)


initCmd: Cmd Msg
initCmd=
  Events.measureSize Resize

update: Msg ->Model ->( Model, Cmd Msg )
update msg model=
  case msg of
    Tick _->
      ( {model
        | world=
            World.simulate (Duration.seconds (1 /60))
              model.world
        }
      , Cmd.none
      )

    Resize width height->
      ( {model
        | width= pixels width
        , height= pixels height
        }
      , Cmd.none
      )

    DropSome cash->
      ( model
      , List.repeat (round ((^) (toFloat cash) 0.7))
          (Random.generate AddRandom randomBody)
        |>Cmd.batch
      )

    AddRandom body->
      ( {model
        | world= World.add body model.world
        }
      , Cmd.none
      )


subscriptions: Model ->Sub Msg
subscriptions _=
  Sub.batch
    [ Events.onResize Resize
    , Events.onAnimationFrameDelta Tick
    ]


view: Model ->Html Msg
view { width, height, world }=
  let
    entities =
      World.bodies world
      |>List.map
          (\body ->
            Scene3d.placeIn
              (Body.frame body)
              (.entity (Body.data body))
          )
  in
  Html.div
    [ Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "left" "0"
    , Html.Attributes.style "top" "0"
    ]
    [ Scene3d.sunny
        { upDirection= Direction3d.positiveZ
        , sunlightDirection=
            Direction3d.xyZ
              (Angle.degrees 135) (Angle.degrees -60)
        , shadows= True
        , camera= camera
        , dimensions=
            ( Pixels.int (round (Pixels.toFloat width))
            , Pixels.int (round (Pixels.toFloat height))
            )
        , background= Scene3d.backgroundColor (Color.rgb 0.06 0.3 0.37) --Scene3d.transparentBackground
        , clipDepth= Length.meters 0.01
        , entities= entities
        }
    ]


initialWorld: World Data
initialWorld=
  World.empty
  |>World.withGravity
      (Acceleration.metersPerSecondSquared 9.80665)
      Direction3d.negativeZ
  |>World.add floor
  |>World.add
      (table
      |>Body.rotateAround Axis3d.y (Angle.radians (pi / 16))
      |>Body.moveTo (Point3d.meters 4 0 1.5)
      )
  |>World.add
      (coin
      |>Body.rotateAround
          (Axis3d.through Point3d.origin
            (Direction3d.unsafe { x= 0.7071, y= 0.7071, z= 0 })
          )
          (Angle.radians (pi /2))
      |>Body.moveTo (Point3d.meters 2.5 0 3)
      )


type Id=
  Floor
  | Table
  | Coin
  | Banknote

type alias Data=
  { entity: Scene3d.Entity BodyCoordinates
  , id: Id
  }


floor: Body Data
floor=
  let
    shape=
      Block3d.centeredOn Frame3d.atOrigin
        ( meters 20, meters 20, meters 0.1 )
  in
  Body.block shape
    { id= Floor
    , entity=
        shape
        |>Scene3d.block
            (Scene3d.Material.matte Color.darkCharcoal)
        |>Scene3d.translateBy
            (Vector3d.meters 0 0 -0.05)
    }

banknote: Body Data
banknote=
  let
    block3d=
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 0.009
        , Length.meters 0.18
        , Length.meters 0.36
        )
  in
  Body.block block3d
    { entity=
        Scene3d.blockWithShadow
          (Scene3d.Material.nonmetal
            { baseColor= Color.rgb 200 250 150
            , roughness= 0.25
            }
          )
          block3d
    , id= Banknote
    }
  |>Body.withBehavior
      (Body.dynamic (Mass.kilograms 0.01))
  |>Body.withMaterial
      (Physics.Material.custom
        { bounciness= 0
        , friction= 0.01
        }
      )


coin: Body Data
coin=
  let
    cylinder3d=
      Cylinder3d.centeredOn Point3d.origin
        Direction3d.x
        { radius= Length.meters 0.1
        , length= Length.meters 0.03
        }
  in
  Body.cylinder cylinder3d
    { id= Coin
    , entity=
        Scene3d.cylinderWithShadow
          (Scene3d.Material.metal
            { baseColor= Color.rgb 160 70 0
            , roughness= 0
            }
          )
          cylinder3d
    }
  |>Body.withBehavior
      (Body.dynamic (Mass.kilograms 0.08))
  |>Body.withMaterial
      (Physics.Material.custom
        { bounciness= 0.19
        , friction= 0.03
        }
      )


table: Body Data
table=
  let
    blocks=
      [ Block3d.from
          (Point3d.meters 1.11 1.11 0)
          (Point3d.meters 1.36 1.36 2.00)
      , Block3d.from
          (Point3d.meters -1.36 1.11 0)
          (Point3d.meters -1.11 1.36 2.00)
      , Block3d.from
          (Point3d.meters -1.36 -1.36 0)
          (Point3d.meters -1.11 -1.11 2.00)
      , Block3d.from
          (Point3d.meters 1.11 -1.36 0)
          (Point3d.meters 1.36 -1.11 2.00)
      , Block3d.from
          (Point3d.meters -1.375 -1.375 2.00)
          (Point3d.meters 1.375 1.375 2.25)
      ]

    shapes=
      blocks
      |>List.map Physics.Shape.block

    entities=
      blocks
      |>List.map
          (Scene3d.blockWithShadow
            (Scene3d.Material.nonmetal
              { baseColor= Color.rgb 150 120 20
              , roughness= 0.2
              }
            )
          )
  in
  Body.compound shapes
    { id= Table
    , entity= Scene3d.group entities
    }
  |>Body.withBehavior
      (Body.dynamic (kilograms 58))
  |>Body.withMaterial
      (Physics.Material.custom
        { bounciness= 0.02
        , friction= 0.02
        }
      )

{-| A random body raised above the plane, shifted or rotated to a random 3d angle.
-}
randomBody: Random.Generator (Body Data)
randomBody=
  Random.map4
    (\angle ( dirX, dirY, dirZ ) ( x, y, z ) body->
      (case body of
        0-> banknote
        1-> coin
        _{-2-}-> coin
      )
      |>Body.rotateAround
          (Axis3d.through Point3d.origin
            (Vector3d.direction
              (Vector3d.from Point3d.origin
                (Point3d.meters dirX dirY dirZ)
              )
            |>Maybe.withDefault Direction3d.x
            )
          )
          (Angle.radians angle)
      |>Body.moveTo (Point3d.meters (4+x) (0+y) (7.2+z))
    )
    (Random.float (-pi /2) (pi /2))--angle
    (Random.map3 (\x y z-> ( x, y, z ))
      (Random.float -1.5 1.5)
      (Random.float -1.5 1.5)
      (Random.float -1 1)
    )
    (Random.map3 (\x y z-> ( x, y, z ))
      (Random.float -1 1)
      (Random.float -1 1)
      (Random.float -0.5 0.5)
    )
    (Random.int 0 2)--body

