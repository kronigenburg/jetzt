module Jetzt exposing (main)

import Element as Ui
import Element.Input as UiInput
import Element.Background as UiBg
import Element.Font as UiFont

import Browser
import Html exposing (Html)
import Coins
import Coins


main: Program () Model Msg
main=
  Browser.document
    { init= init
    , view= view
    , update= update
    , subscriptions= subscriptions
    }

type alias Model=
  { investmentCounter: Int
  , donationCounter: Int
  , coins: Coins.Model
  }

type Msg=
  ClickDonate
  | ClickInvest
  | CoinsMsg Coins.Msg

init: () ->( Model, Cmd Msg )
init flags=
  ( { investmentCounter= 0
    , donationCounter= 0
    , coins= Coins.initModel
    }
  , Coins.initCmd |>Cmd.map CoinsMsg
  )

subscriptions: Model ->Sub Msg
subscriptions model=
  Coins.subscriptions model.coins
  |>Sub.map CoinsMsg

update: Msg ->Model ->( Model, Cmd Msg )
update msg model =
  case msg of
    ClickDonate->
      ( {model
        | donationCounter=
            1 +round ((toFloat model.donationCounter)  * 1.6)
        }
      , Cmd.none
      )

    ClickInvest->
      update (CoinsMsg Coins.Random)
        {model
        | investmentCounter=
            1 +round ((toFloat model.investmentCounter)  * 1.6)
        }
    
    CoinsMsg coinsMsg->
      Coins.update coinsMsg model.coins
      |>Tuple.mapBoth
          (\coinsModel-> {model | coins= coinsModel })
          (Cmd.map CoinsMsg)


view: Model ->Browser.Document Msg
view model=
  { title= "jetzt"
  , body=
      [ Ui.layout []
          (Ui.column
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Ui.behindContent
                (Ui.html
                  (Coins.view model.coins
                  |>Html.map CoinsMsg
                  )
                )
            ]
            [ Ui.el
                [ Ui.height (Ui.px 36)
                , Ui.paddingXY 10 0
                , UiFont.color (Ui.rgb 0.2 0.2 0.2)
                ]
                (Ui.el [ Ui.centerY, UiFont.color (Ui.rgb 1 1 1) ]
                  (Ui.text "🔐 secure")
                )
            , Ui.row
                [ Ui.height Ui.fill
                , Ui.width Ui.fill
                ]
                [ Ui.el
                    [ Ui.height Ui.fill
                    , Ui.paddingXY 0 200
                    , Ui.width (Ui.fillPortion 10)
                    , UiBg.gradient
                        { angle= 0
                        , steps=
                            [ Ui.rgba 0 0.06 0.05 0.2
                            , Ui.rgba 0 0 0.02 0.2
                            , Ui.rgba 0 0 0 0.2
                            ]
                        }
                    ]
                    viewHeader
                , Ui.column
                    [ Ui.height Ui.fill
                    , Ui.width (Ui.fillPortion 11)
                    , Ui.paddingXY 88 178
                    , UiBg.gradient
                        { angle= 180
                        , steps=
                            [ Ui.rgba 0 0.06 0.05 0.4
                            , Ui.rgba 0 0 0.02 0.4
                            , Ui.rgba 0 0 0 0.4
                            ]
                        }
                    ]
                    [ viewIntroduction
                    , Ui.image []
                        { src= "https://media.istockphoto.com/photos/cash-money-picture-id173575156?k=6&m=173575156&s=612x612&w=0&h=1r1Q_6l66i5qHsm6zMEJYoDPhU83QsKms6l30XQXMws="
                        , description= "image of money"
                        }
                    , viewDonateInvest model
                    ]
                ]
            ]
          )
      ]
  }

viewHeader: Ui.Element Msg
viewHeader=
  Ui.column
    [ Ui.width Ui.fill
    , Ui.paddingXY 0 20
    ]
    [ Ui.el
        [ UiFont.size 72
        , UiFont.color (Ui.rgb 1 1 1)
        , UiFont.hairline
        , UiFont.family [ UiFont.typeface "Ubuntu Mono" ]
        , Ui.centerX
        ]
        (Ui.text "Kronigenburg")
    , Ui.el
        [ UiFont.size 20
        , UiFont.color (Ui.rgb 0.5 0.8 0.1)
        , UiFont.hairline
        , Ui.centerX
        ]
        (Ui.text "👑 the  e s s e n c e")
    ]

viewIntroduction: Ui.Element msg
viewIntroduction=
  Ui.column
    [ Ui.width Ui.fill
    ]
    [ Ui.el
        [ Ui.padding 7
        , UiFont.color (Ui.rgb 0 0.7 0.5)
        , UiFont.size 26
        ]
        (Ui.text "Du möchtest das schnelle Geld?")
    , Ui.el
        [ UiFont.color (Ui.rgb 0.7 0.5 0)
        , UiFont.size 26
        , UiFont.bold
        ]
        (Ui.text "      Die Zeit ist jetzt!")
    , Ui.el
        [ Ui.padding 7
        , UiFont.color (Ui.rgb 0.7 0.5 0)
        , UiFont.size 36
        ]
        (Ui.text "Handle schnell, bevor es zu spät ist!")
    ]

viewDonateInvest: Model ->Ui.Element Msg
viewDonateInvest model=
  Ui.column
    [ Ui.width Ui.fill
    ]
    [ Ui.row [ Ui.spacing 12 ]
        [ UiInput.button
              [ UiBg.color (Ui.rgb 0.8 0 0)
              ]
              { onPress= Just ClickInvest
              , label=
                  Ui.el
                    [ Ui.paddingXY 20 20
                    , UiFont.color (Ui.rgb 1 1 1)
                    , UiFont.bold
                    ]
                    (Ui.text "Investieren")
              }
          , Ui.el
              [ UiFont.color (Ui.rgb 1 1 1) 
              , UiFont.family [ UiFont.typeface "Swadasee" ]
              ]
              (Ui.text 
                ("💎: "
                ++(String.fromInt model.investmentCounter)
                ++"€  bisher"
                )
              )
        ]
    , Ui.row [ Ui.spacing 12 ]
        [ UiInput.button
            [ UiBg.color (Ui.rgb 0 0.6 0)
            ]
            { onPress= Just ClickDonate
            , label=
                Ui.el
                  [ Ui.paddingXY 20 20
                  , UiFont.color (Ui.rgb 1 1 1)
                  , UiFont.bold
                  ]
                  (Ui.text "Spenden")
            }
        , Ui.el
            [ UiFont.color (Ui.rgb 1 1 1) 
            , UiFont.family [ UiFont.typeface "Swadasee" ]
            ]
            (Ui.text
              ("🎁: "
              ++(String.fromInt model.donationCounter)
              ++"€  bisher"
              )
            )
        ]
    ]

    