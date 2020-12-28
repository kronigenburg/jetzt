module Coins exposing
  ( Model, Msg(..), initModel, initCmd
  , view, update, subscriptions
  )


import Html exposing (Html)
import Html.Events exposing (onClick)

import Acceleration
import Angle
import Axis3d
import Block3d
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
import Length
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Shape as Shape
import Physics.World as World exposing (World)
import Point3d
import Sphere3d
import Vector3d

import Random


type alias Model=
  { world: World Meshes
  , fps: List Float
  , settings: Settings
  , camera: Camera
  }

initModel: Model
initModel=
  { world= initialWorld
  , fps= []
  , settings= {settings | showSettings= True }
  , camera=
      Camera.camera
        { from= { x= 0, y= 30, z= 20 }
        , to= { x= 0, y= 0, z= 0 }
        }
  }

type Msg=
  ForSettings SettingsMsg
  | Tick Float
  | Resize Float Float
  | Restart
  | Random
  | AddRandom (Body Meshes)


initCmd: Cmd Msg
initCmd=
  Events.measureSize Resize

update: Msg ->Model ->( Model, Cmd Msg )
update msg model=
  case msg of
    ForSettings settingsMsg->
      ( {model
        | settings=
            Settings.update settingsMsg
              model.settings
        }
      , Cmd.none
      )

    Tick dt->
      ( {model
        | fps= Fps.update dt model.fps
        , world=
            World.simulate (Duration.seconds (1 / 60))
              model.world
        }
      , Cmd.none
      )

    Resize width height->
      ( {model
        | camera= Camera.resize width height model.camera
        }
      , Cmd.none
      )

    Restart->
      ( {model | world= initialWorld }
      , Cmd.none
      )

    Random->
      ( model
      , Random.generate AddRandom randomBody
      )

    AddRandom body->
      ( {model | world= World.add body model.world }
      , Cmd.none
      )


subscriptions: Model ->Sub Msg
subscriptions _=
  Sub.batch
    [ Events.onResize Resize
    , Events.onAnimationFrameDelta Tick
    ]


view: Model ->Html Msg
view { settings, fps, world, camera }=
  Html.div []
    [ Scene.view
        { settings= settings
        , world= world
        , camera= camera
        , meshes= identity
        , maybeRaycastResult= Nothing
        , floorOffset= floorOffset
        }
    ]


initialWorld: World Meshes
initialWorld=
  World.empty
  |>World.withGravity
    (Acceleration.metersPerSecondSquared 9.80665)
    Direction3d.negativeZ
  |>World.add floor
  |>World.add
      (box
      |>Body.rotateAround Axis3d.y (Angle.radians (-pi / 5))
    |>Body.moveTo (Point3d.meters 0 0 2)
      )
  |>World.add
      (sphere
      |>Body.moveTo (Point3d.meters 0.5 0 8)
      )
  |>World.add
      (cylinder
      |>Body.rotateAround
          (Axis3d.through Point3d.origin
            (Direction3d.unsafe { x= 0.7071, y= 0.7071, z= 0 })
          )
          (Angle.radians (pi / 2))
      |>Body.moveTo (Point3d.meters 0.5 0 11)
      )
  |>World.add
      (compound
      |>Body.rotateAround
          (Axis3d.through Point3d.origin
            (Direction3d.unsafe { x= 0.7071, y= 0.7071, z= 0 })
          )
          (Angle.radians (pi / 5))
      |>Body.moveTo (Point3d.meters -1.2 0 5)
      )


{-| Shift the floor a little bit down.
-}
floorOffset: { x: Float, y: Float, z: Float }
floorOffset=
  { x= 0, y= 0, z= -1 }


{-| Floor has an empty mesh, because it is not rendered
-}
floor: Body Meshes
floor=
  Body.plane (Meshes.fromTriangles [])
  |>Body.moveTo (Point3d.fromMeters floorOffset)


box: Body Meshes
box=
  let
    block3d=
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 1
        , Length.meters 1
        , Length.meters 0.5
        )
  in
  Body.block block3d
    (Meshes.fromTriangles (Meshes.block block3d))
  |>Body.withBehavior
      (Body.dynamic (Mass.kilograms 1))


sphere: Body Meshes
sphere=
  let
    sphere3d=
      Sphere3d.atOrigin (Length.meters 0.4)
  in
  Body.sphere
    sphere3d
    (Meshes.fromTriangles (Meshes.sphere 2 sphere3d))
  |>Body.withBehavior
      (Body.dynamic (Mass.kilograms 1))


cylinder: Body Meshes
cylinder=
  let
    cylinder3d=
      Cylinder3d.centeredOn Point3d.origin
        Direction3d.x
        { radius= Length.meters 0.25
        , length= Length.meters 0.8
        }
  in
  Body.cylinder cylinder3d
    (Meshes.fromTriangles (Meshes.cylinder 16 cylinder3d))
  |>Body.withBehavior (Body.dynamic (Mass.kilograms 5))


{-| A compound body made of three boxes.
-}
compound: Body Meshes
compound=
  let
    blocks=
      List.map
        (\center->
          Block3d.centeredOn
            (Frame3d.atPoint center)
            ( Length.meters 0.5
            , Length.meters 0.5
            , Length.meters 0.5
            )
        )
        [ Point3d.meters -0.25 0 -0.25
        , Point3d.meters -0.25 0 0.25
        , Point3d.meters 0.25 0 0.25
        ]
  in
  Body.compound
    (List.map Shape.block blocks)
    (Meshes.fromTriangles (List.concatMap Meshes.block blocks))
  |>Body.withBehavior (Body.dynamic (Mass.kilograms 1))


{-| A random body raised above the plane, shifted or rotated to a random 3d angle.
-}
randomBody: Random.Generator (Body Meshes)
randomBody=
  Random.map5
    (\angle x y z body->
      (case body of
        0-> box

        1-> sphere

        2-> cylinder

        _ {-3-}-> compound
      )
      |>Body.rotateAround
          (Axis3d.through Point3d.origin
            (Maybe.withDefault Direction3d.x
              (Vector3d.direction
                (Vector3d.from Point3d.origin (Point3d.meters x y z))
              )
            )
          )
          (Angle.radians angle)
      |>Body.moveTo (Point3d.meters 8 0 10)
    )
    (Random.float (-pi /2) (pi /2))--angle
    (Random.float -2 2)--x
    (Random.float -2 2)--y
    (Random.float -1 1)--z
    (Random.int 0 3)--body

