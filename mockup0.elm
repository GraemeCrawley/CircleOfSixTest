import GraphicSVG exposing (..)
import Image.Collection as Collection exposing (Key, Collection)
import Image exposing (Image)

main = gameApp Tick { model = init, view = view, update = update }

-- MODEL

type State = Start | ExpandingContacts | NoteFadingIn | NoteFadingOut | StationaryNoNote | StationaryWithNote

init = { time = 0, state = StationaryNoNote, noteTransparency = 0, aniTimeFade = 0, aniTimeExpand = 0, spiralTheta = 0, circleOfSixX = 0, circleOfSixY = 0 }

update msg model = case msg of
                    Tick t _     -> { model | time = t 
                                        , aniTimeFade = case model.state of
                                                    NoteFadingIn -> model.aniTimeFade + 0.05
                                                    NoteFadingOut -> model.aniTimeFade + 0.05
                                                    _ -> 0
                                        , aniTimeExpand = case model.state of
                                                    Start -> 0.5
                                                    ExpandingContacts -> model.aniTimeExpand + 0.05
                                                    _ -> 20
                                        , spiralTheta = case model.state of
                                                    Start -> 0.5
                                                    ExpandingContacts -> model.spiralTheta + 0.05
                                                    _ -> 20
                                        , state = updateState model
                                        , noteTransparency = updateNoteTransparency model
                                        {- UNECESSARY FOR NOW
                                        --, circleOfSixX = updateExpandingContactsX model
                                        --, circleOfSixY = updateExpandingContactsY model
                                        -}
                                        }
                    ButtonPress -> { model | state = buttonPress model.state 
                                    }
                    ContactsPress -> { model | state = contactsPress model.state 
                                    }

                    XPress -> { model | state = xPress model.state 
                                    }


updateState model = case model.state of
                        NoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryWithNote
                                    else NoteFadingIn
                        NoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else NoteFadingOut
                        ExpandingContacts -> if model.aniTimeExpand >= 20
                                    then StationaryNoNote
                                    else ExpandingContacts
                        StationaryWithNote -> StationaryWithNote
                        StationaryNoNote -> StationaryNoNote
                        Start -> Start


updateNoteTransparency model = case model.state of
                        NoteFadingIn -> model.noteTransparency + 0.1
                        NoteFadingOut -> model.noteTransparency - 0.1
                        _ -> model.noteTransparency

{- UNNECESSARY FOR NOW
updateExpandingContactsX model = case model.state of
                        -- USE FUNCTION TO CHANGE SPEED OF X
                        ExpandingContacts -> model.circleOfSixX
                        _ -> model.circleOfSixX

updateExpandingContactsY model = case model.state of
                        -- USE FUNCTION TO CHANGE SPEED OF Y
                        ExpandingContacts -> model.circleOfSixY
                        _ -> model.circleOfSixY
-}



-- UPDATE
type Msg = Tick Float GetKeyState
        | ContactsPress
        | ButtonPress
        | XPress

buttonPress oldState = case oldState of
                        Start -> Start
                        ExpandingContacts -> ExpandingContacts
                        StationaryNoNote -> NoteFadingIn
                        NoteFadingIn -> NoteFadingIn
                        NoteFadingOut -> NoteFadingOut
                        StationaryWithNote -> StationaryWithNote

xPress oldState = case oldState of
                        Start -> Start
                        ExpandingContacts -> ExpandingContacts
                        StationaryWithNote -> NoteFadingOut
                        NoteFadingOut -> NoteFadingOut
                        NoteFadingIn -> NoteFadingIn
                        StationaryNoNote -> StationaryNoNote

contactsPress oldState = case oldState of
                        Start -> ExpandingContacts
                        ExpandingContacts -> ExpandingContacts
                        StationaryWithNote -> StationaryWithNote
                        NoteFadingOut -> NoteFadingOut
                        NoteFadingIn -> NoteFadingIn
                        StationaryNoNote -> StationaryNoNote



-- VIEW

view model = collage 250 128 (myShapes model)

-- CONTACT ANIMATION CONTROLS
--    Allows control of the contact bubbles animation

-- Original scaled size of each
scaledSize = 1
-- Time between each piece popping out
timeShift = 0.1
-- Size to pop out to
enlargedSize = 1.05

stringOfAniTime x = Basics.toString x

--Functions to animate the movement
-- NEW
contactPoint1 = 3
speedOfSpiral ani = if ani < contactPoint1 then 1/(1+Basics.e^((-1)*ani))
                        else 1/(1+Basics.e^((-1)*contactPoint1))

inverseTheta x = 50/x
a = 10
b = 1

circleX x ani = if ani < 1.7 then (cos (ani+1.5))*10
                    else if ani < 3.2 then (sin (ani))*10-20
                        else if ani < 10 then (cos (ani-1.5))*10-20
                            else (cos (10-1.5))*10-20

circleY y ani = if ani < 1.7 then (sin (ani+1.5))*10
                    else if ani < 3.2 then (cos (ani))*10+1
                        else if ani < 10 then (sin (ani-1.5))*10-20
                            else (sin (10-1.5))*10-20

myShapes model = [-- BACKGROUND
             rect 250 128 |> filled black
           , rect 248 126 |> filled (rgb 253 255 219)
           , graphPaper 10 |> makeTransparent 0.4
           , profilePerson
                |> move (0,-20)
            , text ("ExpandTime " ++ (stringOfAniTime model.aniTimeExpand))
                |> filled black
                |> move (30, -50)
            -- Top
           , myCircle
               |> notifyTap ButtonPress
               |> move (0,(10))         --speed, % of the wheel
               |> scale (max(sin model.time*enlargedSize) scaledSize)
            -- Right Top
           , myCircle
               |> move (26,(15-20))
               |> scale (max(sin (model.time-timeShift*1)*enlargedSize) scaledSize)
            -- Right Bottom
           , myCircle
               |> move (26,(-15-20))
               |> scale (max(sin (model.time-timeShift*2)*enlargedSize) scaledSize)
            -- Bottom
           , myCircle
               |> move (0,(-30-20))
               |> scale (max(sin (model.time-timeShift*3)*enlargedSize) scaledSize)
            -- Left Top
           , myCircle
               |> move (-26,(-15-20))
               |> scale (max(sin (model.time-timeShift*4)*enlargedSize) scaledSize)
            -- Left Bottom
           , myCircle
               |> move (-26,(15-20))
               |> scale (max(sin (model.time-timeShift*5)*enlargedSize) scaledSize)
           , roundedRect 80 20 5
               |> filled noteColour
               |> move (0, 35)
               |> makeTransparent model.noteTransparency
           , text text1
               |> filled black
               |> scale 0.45
               |> move (-35 , 38)
               |> makeTransparent model.noteTransparency
           , closeBoxSymbol
               |> rotate (degrees 45)
               |> move (35, 40)
               |> makeTransparent model.noteTransparency
               |> notifyTap XPress
           ]
                -- END OF MODEL

-- DEFINING VARIABLES

positive x = if x > 0 then x else 0


text1 = "hello1"


profilePerson = myCircle

textBox1 = group [roundedRect 80 20 5
                |> filled yellow
           , text "Journal Entry:"
                |> filled black
                |> scale 0.45
                |> move (-35 , 0)
            , closeBoxSymbol
                |> rotate (degrees 45)
                |> move (35, 5)
            ]

closeBoxSymbol = group [circle 3
                |> filled noteColour
           , rect 1 5
                |> filled black
           , rect 5 1
                |> filled black
            ]
                

backgroundPerson = rgb 200 80 75
contactPersonColour = rgb 67 157 145 
noteColour = rgb 250 166 106   

myCircle = group [circle 10
                         |> filled backgroundPerson,
                -- Main body
                curve (0,0) [Pull (5,10) (10,0)]
                |> filled contactPersonColour
                |> move (-5,-8),
                -- Head
                circle 4
                |> filled contactPersonColour
                |> move (0,0),
                -- Left Side
                circle 2
                |> filled contactPersonColour
                |> move (0,-8.1),
                circle 2
                |> filled contactPersonColour
                |> move (-1,-7.9),
                circle 2
                |> filled contactPersonColour
                |> move (-2,-7.8),
                circle 1
                |> filled contactPersonColour
                |> move (-3.8,-8.1),
                circle 0.5
                |> filled contactPersonColour
                |> move (-4.5,-8.1),
                circle 0.3
                |> filled contactPersonColour
                |> move (-4.9,-8.4),
                circle 0.3
                |> filled contactPersonColour
                |> move (-4.7,-8.6),
                circle 0.5
                |> filled contactPersonColour
                |> move (-3.5,-8.8),
                -- Right Side
                circle 2
                |> filled contactPersonColour
                |> move (0,-8.1),
                circle 2
                |> filled contactPersonColour
                |> move (1,-7.9),
                circle 2
                |> filled contactPersonColour
                |> move (2,-7.8),
                circle 1
                |> filled contactPersonColour
                |> move (3.8,-8.1),
                circle 0.5
                |> filled contactPersonColour
                |> move (4.5,-8.1),
                circle 0.3
                |> filled contactPersonColour
                |> move (4.9,-8.4),
                circle 0.3
                |> filled contactPersonColour
                |> move (4.7,-8.6),
                circle 0.5
                |> filled contactPersonColour
                |> move (3.5,-8.8)                   
                ]









