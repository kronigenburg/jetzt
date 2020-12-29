module Jetzt exposing (main)

import Element as Ui
import Element.Input as UiInput
import Element.Background as UiBg
import Element.Font as UiFont
import Element.Events as UiEvent

import Browser
import Html exposing (Html)
import Coins
import Coins
import Element.Region exposing (description)


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
  , mouseOverInvest: MouseOver
  , donationCounter: Int
  , coins: Coins.Model
  }

type Msg=
  ClickDonate
  | ClickInvest
  | ChangeInvestMouseOver MouseOver
  | CoinsMsg Coins.Msg

init: () ->( Model, Cmd Msg )
init flags=
  ( { investmentCounter= 0
    , mouseOverInvest= MouseOutside
    , donationCounter= 0
    , coins= Coins.initModel
    }
  , Coins.initCmd |>Cmd.map CoinsMsg
  )

subscriptions: Model ->Sub Msg
subscriptions model=
  Coins.subscriptions model.coins
  |>Sub.map CoinsMsg

type MouseOver=
  MouseInside
  | MouseOutside

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
      let
        cash=
          1 +round ((toFloat model.investmentCounter)  * 0.6)
      in
      update (CoinsMsg (Coins.DropSome cash))
        {model
        | investmentCounter= cash +model.investmentCounter
        }
        
    ChangeInvestMouseOver mouseOver->
      ( {model
        | mouseOverInvest= mouseOver
        }
      , Cmd.none
      )
    
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
            [ Ui.row
                [ Ui.height Ui.fill
                , Ui.width Ui.fill
                ]
                [ Ui.column
                    [ Ui.height Ui.fill
                    , Ui.width (Ui.fillPortion 10)
                    ]
                    [ Ui.el
                        [ Ui.height (Ui.px 36)
                        , Ui.paddingXY 10 0
                        , UiFont.color (Ui.rgb 0.2 0.2 0.2)
                        ]
                        (Ui.el
                          [ Ui.centerY, UiFont.color (Ui.rgb 0.9 0.9 0.9) ]
                          (Ui.text "🔐 secure")
                        )
                    , Ui.el
                        [ Ui.height Ui.fill
                        , Ui.height Ui.fill
                        , Ui.paddingXY 46 160
                        , UiBg.gradient
                            { angle= pi/2
                            , steps=
                                [ Ui.rgba 0 0.06 0.05 0.2
                                , Ui.rgba 0 0 0.02 0.2
                                , Ui.rgba 0 0 0 0
                                ]
                            }
                        ]
                        viewHeader
                    ]
                , Ui.column
                    [ Ui.height Ui.fill
                    , Ui.width (Ui.fillPortion 11)
                    , Ui.paddingXY 88 120
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
                    , Ui.image
                        [ Ui.alpha 0.5
                        , Ui.onRight
                            (Ui.image [ Ui.alpha 0.5 ]
                              { src= "https://i.makeagif.com/media/10-11-2015/weRwzr.gif"
                              , description= "counting money"
                              }
                            )
                        ]
                        { src= "https://www.reactiongifs.com/r/mny1.gif"
                        , description= "money"
                        }
                    , Ui.image
                        [ Ui.alpha 0.5
                        , Ui.onRight
                            (Ui.image [ Ui.alpha 0.5 ]
                              { src= "https://66.media.tumblr.com/0fe183cffc65bb489d9788846958ae19/tumblr_phdfdjgGE91v9enyp_540.gif"
                              , description= "more money"
                              }
                            )
                        ]
                        { src= "https://media.giphy.com/media/b5hW2FN09dXlm/giphy.gif"
                        , description= "walking money"
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
    , Ui.paddingXY 0 29
    ]
    [ Ui.el
        [ UiFont.size 72
        , UiFont.color (Ui.rgb 1 1 1)
        , UiFont.hairline
        , UiFont.family [ UiFont.typeface "Ubuntu Mono" ]
        , Ui.centerX
        ]
        (Ui.text "👑 Kronigenburg")
    , Ui.el
        [ UiFont.size 30
        , UiFont.color (Ui.rgb 0.65 0.65 1)
        , UiFont.hairline
        , Ui.centerX
        , UiFont.family [ UiFont.typeface "Noto Sans" ]
        ]
        (Ui.text "      e s s e n c e      of  Life")
    ]

viewIntroduction: Ui.Element msg
viewIntroduction=
  Ui.column
    [ Ui.width Ui.fill
    ]
    [ Ui.el
        [ Ui.padding 7
        , UiFont.color (Ui.rgb 0 0.89 0.62)
        , UiFont.size 26
        ]
        (Ui.text "Du möchtest das schnelle Geld?")
    , Ui.el
        [ UiFont.color (Ui.rgb 0.89 0.52 0)
        , UiFont.size 26
        , UiFont.bold
        ]
        (Ui.text "      Die Zeit ist jetzt!")
    , Ui.el
        [ Ui.padding 7
        , UiFont.color (Ui.rgb 0.89 0.62 0)
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
                case model.mouseOverInvest of
                  MouseInside->
                    Ui.el
                      [ Ui.paddingXY 24 20
                      , UiBg.image "https://img.washingtonpost.com/news/get-there/wp-content/uploads/sites/37/2016/08/0819_cash2_ss.gif"
                      , UiBg.color  (Ui.rgb 0.4 0.6 1)
                      , UiEvent.onMouseLeave (ChangeInvestMouseOver MouseOutside)
                      , UiFont.color (Ui.rgba 0 0 0 0)
                      ]
                      (Ui.text "Investieren")
                  
                  MouseOutside->
                    Ui.el
                      [ Ui.paddingXY 20 20
                      , UiFont.color (Ui.rgb 0 0 0)
                      , UiFont.bold
                      , UiBg.color (Ui.rgb 0.4 0.6 1)
                      , UiEvent.onMouseEnter (ChangeInvestMouseOver MouseInside)
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

    