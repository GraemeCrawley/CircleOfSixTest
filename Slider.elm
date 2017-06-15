import GraphicSVG exposing (..)

main = gameApp Tick { model = init, view = view, update = update }

view model = collage 250 128 (myShapes model)
myShapes model = [ 
                -- BACKGROUND
                rect 250 128 |> filled black
                ,rect 248 126 |> filled (rgb 253 255 219)

                --everything but background 
                ,group [  
                -- CALENDAR
                  group [ 
                    rect 22 30
                       |> filled black
                       |> move (0, 4)
                  ,  rect 20 20 
                      |> filled (rgb 249 184 161) 
                      |> move (0,8)
                  , rect 20 20 
                      |> filled white
                  , text "MAY 31" 
                     |> filled black
                      |> scale 0.45
                      |> move (-9.5,12)
                      ] |> move (-99, -42)

                --FACE
                ,group [
                    face 
                      |> scale 0.5 
                      |> move (-50,0)
                    -- GROUP EYEBROWS FOR MOVEMENT
                    ,group [
                         eyebrow 
                          |> move (-65,12) 
                          |> rotate (degrees 5)
                          |> scale 0.55 
                       --   |> rotate (degrees (clamp 0 50 (0.4*model.happiness)) )
                        ,eyebrow 
                          |> move (-35,12) 
                          |> rotate (degrees 175)
                          |> scale 0.55
                         -- |> rotate (degrees (clamp 0 -50 (-0.4*model.happiness))) 
                           ] |> move (0, (model.happiness) * -0.05)

                   ,curve (-50,-20) [Pull (0,model.happiness - 20) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (-50,-15)
                      |> scale 0.4
                   ,group [ -- Text under head  
                         rect 55 15 
                          |> filled white
                          |> move (-52, -50)
                        ,rect 55 0.6 
                          |> filled (rgb 204 204 255)
                          |> move (-52, -53.25)
                        ,rect 55 0.6 
                          |> filled (rgb 204 204 255)
                          |> move (-52, -48)  
                        ,text "Journal Entry:"
                          |> filled black
                          |> scale 0.45
                          |> move (-66, -48)  
                        ,text "I feel happy/upset because"
                          |> filled black
                          |> scale 0.4
                          |> move (-77, -53)
                        ,rect 0.35 3.3
                         |> filled black 
                         |> move (-25.5, -51.6)
                         |> makeTransparent (cos (model.time * 7))  
                          ] |> move (0,-2)
                        ] |> move (model.xcoord,model.ycoord)
                          |> scale model.size

                -- SLIDER
                , group [ 
                    circle 10 
                      |> filled (rgb 211 162 184)
                      |> move (model.happiness,0)
                  ,roundedRect 100 10 5
                      |> filled (rgba 187 130 161 0.4)
                      |> notifyMouseMoveAt Slider
                        ] |> scale 0.7 |> move (40,-30)
                   
                   
              , text "How do you feel?"
                      |> filled black --outlined (solid 0.2) (rgb 231 124 149)
              , text "Great" 
                      |> filled black 
                      |> scale 0.5 
                      |> move (0,-40)
              , text "Not so good" 
                      |> filled black 
                      |> scale 0.5 
                      |> move (60,-40)

              , group [
                  roundedRect 25 12 5 
                      |> filled (rgb 249 184 161)
                      |> move (40,-50)
                 ,text "SUBMIT" 
                      |> filled black
                      |> scale 0.4
                      |> move (31,-52)
                      ] |> notifyTap ButtonPress
              
                    ] |> move (0,8)
            --Happiness Counter
            --   , text (toString <| round <| model.happiness)
              -- |> filled black
              -- |> move (0,20)

                ]
                -- END OF MODEL

{- ALL OF THE CODE FOR THE FACE -}
face = group [
           ear |> move (-70,0) 
          ,ear |> move (70,0)
          ,oval 143 156
            |> filled black 
            |> move (0,-7) 
          ,oval 140 153
            |> filled skin 
            |> move (0,-7)
         , blush |> move (-40,-20)
         , blush |> move (40,-20)
         , eyes 
         , hair |> move(0,60)
             ]

eyes = group [
         filled black (circle 20)|> move (-27,0) --Left eye
        ,filled white (circle 18) |> move (-27,0)
        ,filled lbrown (circle 15) |> move (-24,0)
        ,filled black (circle 12) |> move (-21,2)
        ,filled white (circle 6) |> move (-18,3)
        ,filled black (circle 20)|> move (27,0) --Right eye
        ,filled white (circle 18) |> move (27,0)
        ,filled lbrown (circle 15) |> move (30,0)
        ,filled black (circle 12) |> move (32,2)
        ,filled white (circle 6) |> move (35,3)
      ]

ear = group[
        oval 24 44 |> filled black
       ,oval 20 40 |> filled skin2
           ]

eyebrow = group [ oval 37 4 |> filled black]     
     
blush = group [circle 15 |> filled pinkie]

hair = group [ 
               oval 60 40 |> filled black |> move(0,5)
              ,oval 60 40 |> filled black |> move (-30,0) |> rotate (degrees 20)
              ,oval 60 40 |> filled black |> move (30,0) |> rotate (degrees 160)
              ,oval 55 35 |> filled black |> move (55,-15) |> rotate (degrees 130)
              ,oval 55 35 |> filled black |> move (-55,-15) |> rotate (degrees 50)
              ]

--COLOURS 
skin    = rgb 214 170 83
skin2   = rgb 225 170 100
lips    = rgb 150 18 18
lbrown  = rgb 79 56 3
pinkie  = rgba 255 158 158 0.45

{- ALL THE COD-}
--1) make a type for the states (unless the state is a number or existing type)
-- MOVING INTO CALENDAR
type State = Moving | Start | Stop


-- (2) implement transition function
--SUBMIT BUTTON
buttonPress oldState = case oldState of
                         Start -> Moving
                         Moving -> Moving
                         Stop -> Stop

-- THE TYPE OF MESSAGE, 
--EITHER MOVE THE SLIDER OF PRESS THE SUBMIT BUTTON
type Msg = Tick Float GetKeyState
        | Slider (Float,Float)
        | ButtonPress
        
update msg model = case msg of

                    Tick t _     -> { model | time = t 
                                         , xcoord = updatex model
                                         , ycoord = updatey model
                                         , aniTime = case model.state of
                                                      Moving -> model.aniTime + 0.05
                                                      _ -> 0
                                         , state = updateState model
                                         , size = updateSize model}

                    Slider (x,y) -> { model | happiness = case model.state of
                                                        Start -> (x - 40)*1.43 -- Keeps the user from being able to
                                                                               -- move the slider after submitting
                                                        _     -> model.happiness


                                    }

                    ButtonPress -> { model | state = buttonPress model.state
                                          
                                    }
                 

init = { time = 0, happiness = 0, state = Start, xcoord = 0, ycoord = 0 , aniTime = 0, size = 1 }
updatex model = case model.state of
                      Moving -> model.xcoord - model.aniTime - 1.2
                      _ -> model.xcoord
updatey model = case model.state of
                      Moving -> model.ycoord - model.aniTime + 0.01
                      _ -> model.ycoord
updateState model = case model.state of
                       Moving -> if model.aniTime >= 2
                                    then Stop
                                    else Moving
                       Stop -> Stop
                       Start -> Start
updateSize model = case model.state of
                      Moving -> model.size - 0.02
                      x -> model.size 

