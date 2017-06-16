import GraphicSVG exposing (..)
import Image.Collection as Collection exposing (Key, Collection)
import Image exposing (Image)

main = gameApp Tick { model = init, view = view, update = update }

-- MODEL

type State = Start 
            | StationaryNoNote 
            | StationaryTopNote 
            | TopNoteFadingIn 
            | TopNoteFadingOut 
            | StationaryRightTopNote 
            | RightTopNoteFadingIn 
            | RightTopNoteFadingOut 
            | StationaryRightBottomNote 
            | RightBottomNoteFadingIn 
            | RightBottomNoteFadingOut 
            | StationaryBottomNote 
            | BottomNoteFadingIn 
            | BottomNoteFadingOut 
            | StationaryLeftBottomNote 
            | LeftBottomNoteFadingIn 
            | LeftBottomNoteFadingOut 
            | StationaryLeftTopNote 
            | LeftTopNoteFadingIn 
            | LeftTopNoteFadingOut

init = { time = 0
        , state = StationaryNoNote
        , topNoteTransparency = 0
        , rightTopNoteTransparency = 0
        , rightBottomNoteTransparency = 0
        , bottomNoteTransparency = 0
        , leftBottomNoteTransparency = 0
        , leftTopNoteTransparency = 0
        , aniTimeFade = 0
        , spiralTheta = 0
        , circleOfSixX = 0
        , circleOfSixY = 0
        , topNoteX = 1000
        , topNoteY = 1000
        , rightTopNoteX = 1000
        , rightTopNoteY = 1000
        , rightBottomNoteX = 1000
        , rightBottomNoteY = 1000
        , bottomNoteX = 1000
        , bottomNoteY = 1000
        , leftBottomNoteX = 1000
        , leftBottomNoteY = 1000
        , leftTopNoteX = 1000
        , leftTopNoteY = 1000 }

update msg model = case msg of
                    Tick t _     -> { model | time = t 
                                        , aniTimeFade = case model.state of
                                                    TopNoteFadingIn -> model.aniTimeFade + 0.05
                                                    TopNoteFadingOut -> model.aniTimeFade + 0.05
                                                    RightTopNoteFadingIn -> model.aniTimeFade + 0.05
                                                    RightTopNoteFadingOut -> model.aniTimeFade + 0.05
                                                    RightBottomNoteFadingIn -> model.aniTimeFade + 0.05
                                                    RightBottomNoteFadingOut -> model.aniTimeFade + 0.05
                                                    BottomNoteFadingIn -> model.aniTimeFade + 0.05
                                                    BottomNoteFadingOut -> model.aniTimeFade + 0.05
                                                    LeftBottomNoteFadingIn -> model.aniTimeFade + 0.05
                                                    LeftBottomNoteFadingOut -> model.aniTimeFade + 0.05
                                                    LeftTopNoteFadingIn -> model.aniTimeFade + 0.05
                                                    LeftTopNoteFadingOut -> model.aniTimeFade + 0.05
                                                    _ -> 0
                                        , state = updateState model
                                        , topNoteTransparency = updateTopNoteTransparency model
                                        , rightTopNoteTransparency = updateRightTopNoteTransparency model
                                        , rightBottomNoteTransparency = updateRightBottomNoteTransparency model
                                        , bottomNoteTransparency = updateBottomNoteTransparency model
                                        , leftBottomNoteTransparency = updateLeftBottomNoteTransparency model
                                        , leftTopNoteTransparency = updateLeftTopNoteTransparency model
                                        , topNoteX = case model.state of
                                                    TopNoteFadingIn -> 0
                                                    TopNoteFadingOut -> 0
                                                    StationaryTopNote -> 0
                                                    _ -> 1000
                                        , topNoteY = case model.state of
                                                    TopNoteFadingIn -> 0
                                                    TopNoteFadingOut -> 0
                                                    StationaryTopNote -> 0
                                                    _ -> 1000
                                        , rightTopNoteX = case model.state of
                                                    RightTopNoteFadingIn -> 0
                                                    RightTopNoteFadingOut -> 0
                                                    StationaryRightTopNote -> 0
                                                    _ -> 1000
                                        , rightTopNoteY = case model.state of
                                                    RightTopNoteFadingIn -> 0
                                                    RightTopNoteFadingOut -> 0
                                                    StationaryRightTopNote -> 0
                                                    _ -> 1000
                                        , rightBottomNoteX = case model.state of
                                                    RightBottomNoteFadingIn -> 0
                                                    RightBottomNoteFadingOut -> 0
                                                    StationaryRightBottomNote -> 0
                                                    _ -> 1000
                                        , rightBottomNoteY = case model.state of
                                                    RightBottomNoteFadingIn -> 0
                                                    RightBottomNoteFadingOut -> 0
                                                    StationaryRightBottomNote -> 0
                                                    _ -> 1000
                                        , bottomNoteX = case model.state of
                                                    BottomNoteFadingIn -> 0
                                                    BottomNoteFadingOut -> 0
                                                    StationaryBottomNote -> 0
                                                    _ -> 1000
                                        , bottomNoteY = case model.state of
                                                    BottomNoteFadingIn -> 0
                                                    BottomNoteFadingOut -> 0
                                                    StationaryBottomNote -> 0
                                                    _ -> 1000
                                        , leftBottomNoteX = case model.state of
                                                    LeftBottomNoteFadingIn -> 0
                                                    LeftBottomNoteFadingOut -> 0
                                                    StationaryLeftBottomNote -> 0
                                                    _ -> 1000
                                        , leftBottomNoteY = case model.state of
                                                    LeftBottomNoteFadingIn -> 0
                                                    LeftBottomNoteFadingOut -> 0
                                                    StationaryLeftBottomNote -> 0
                                                    _ -> 1000
                                        , leftTopNoteX = case model.state of
                                                    LeftTopNoteFadingIn -> 0
                                                    LeftTopNoteFadingOut -> 0
                                                    StationaryLeftTopNote -> 0
                                                    _ -> 1000
                                        , leftTopNoteY = case model.state of
                                                    LeftTopNoteFadingIn -> 0
                                                    LeftTopNoteFadingOut -> 0
                                                    StationaryLeftTopNote -> 0
                                                    _ -> 1000

                                        {- UNECESSARY FOR NOW
                                        --, circleOfSixX = updateExpandingContactsX model
                                        --, circleOfSixY = updateExpandingContactsY model
                                        -}
                                        }
                    ButtonPressTop -> { model | state = buttonPressTop model.state 
                                    }
                    ButtonPressRightTop -> { model | state = buttonPressRightTop model.state 
                                    }
                    ButtonPressRightBottom -> { model | state = buttonPressRightBottom model.state 
                                    }
                    ButtonPressBottom -> { model | state = buttonPressBottom model.state 
                                    }
                    ButtonPressLeftBottom -> { model | state = buttonPressLeftBottom model.state 
                                    }
                    ButtonPressLeftTop -> { model | state = buttonPressLeftTop model.state 
                                    }
                    XPressTop -> { model | state = xPressTop model.state 
                                    }
                    XPressRightTop -> { model | state = xPressRightTop model.state 
                                    }
                    XPressRightBottom -> { model | state = xPressRightBottom model.state 
                                    }
                    XPressBottom -> { model | state = xPressBottom model.state 
                                    }
                    XPressLeftBottom -> { model | state = xPressLeftBottom model.state 
                                    }
                    XPressLeftTop -> { model | state = xPressLeftTop model.state 
                                    }


updateState model = case model.state of
                        TopNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryTopNote
                                    else TopNoteFadingIn
                        TopNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else TopNoteFadingOut
                        RightTopNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryRightTopNote
                                    else RightTopNoteFadingIn
                        RightTopNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else RightTopNoteFadingOut
                        RightBottomNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryRightBottomNote
                                    else RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else RightBottomNoteFadingOut
                        BottomNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryBottomNote
                                    else BottomNoteFadingIn
                        BottomNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryLeftBottomNote
                                    else LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> if model.aniTimeFade >= 0.4
                                    then StationaryLeftTopNote
                                    else LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> if model.aniTimeFade >= 0.4
                                    then StationaryNoNote
                                    else LeftTopNoteFadingOut
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote
                        StationaryNoNote -> StationaryNoNote
                        Start -> Start


updateTopNoteTransparency model = case model.state of
                        TopNoteFadingIn -> model.topNoteTransparency + 0.1
                        TopNoteFadingOut -> model.topNoteTransparency - 0.1
                        _ -> model.topNoteTransparency

updateRightTopNoteTransparency model = case model.state of
                        RightTopNoteFadingIn -> model.rightTopNoteTransparency + 0.1
                        RightTopNoteFadingOut -> model.rightTopNoteTransparency - 0.1
                        _ -> model.rightTopNoteTransparency

updateRightBottomNoteTransparency model = case model.state of
                        RightBottomNoteFadingIn -> model.rightBottomNoteTransparency + 0.1
                        RightBottomNoteFadingOut -> model.rightBottomNoteTransparency - 0.1
                        _ -> model.rightBottomNoteTransparency

updateBottomNoteTransparency model = case model.state of
                        BottomNoteFadingIn -> model.bottomNoteTransparency + 0.1
                        BottomNoteFadingOut -> model.bottomNoteTransparency - 0.1
                        _ -> model.bottomNoteTransparency

updateLeftBottomNoteTransparency model = case model.state of
                        LeftBottomNoteFadingIn -> model.leftBottomNoteTransparency + 0.1
                        LeftBottomNoteFadingOut -> model.leftBottomNoteTransparency - 0.1
                        _ -> model.leftBottomNoteTransparency


updateLeftTopNoteTransparency model = case model.state of
                        LeftTopNoteFadingIn -> model.leftTopNoteTransparency + 0.1
                        LeftTopNoteFadingOut -> model.leftTopNoteTransparency - 0.1
                        _ -> model.leftTopNoteTransparency

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
        | ButtonPressTop
        | XPressTop
        | ButtonPressRightTop
        | XPressRightTop
        | ButtonPressRightBottom
        | XPressRightBottom
        | ButtonPressBottom
        | XPressBottom
        | ButtonPressLeftBottom
        | XPressLeftBottom
        | ButtonPressLeftTop
        | XPressLeftTop


buttonPressTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> TopNoteFadingIn
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightTopNoteFadingIn
                        RightTopNoteFadingOut -> RightTopNoteFadingOut
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryTopNote -> TopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote

buttonPressRightTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> RightTopNoteFadingIn
                        RightTopNoteFadingIn -> RightTopNoteFadingIn
                        RightTopNoteFadingOut -> RightTopNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> RightTopNoteFadingOut
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote
   

buttonPressRightBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> RightBottomNoteFadingIn  
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> RightBottomNoteFadingOut
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote                     

buttonPressBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> BottomNoteFadingIn  
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> BottomNoteFadingOut
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote

buttonPressLeftBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> LeftBottomNoteFadingIn  
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> LeftBottomNoteFadingOut
                        StationaryLeftTopNote -> StationaryLeftTopNote

buttonPressLeftTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> LeftTopNoteFadingIn  
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> LeftTopNoteFadingOut

xPressTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightTopNoteFadingIn
                        RightTopNoteFadingOut -> RightTopNoteFadingOut
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryTopNote -> TopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote

xPressRightTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        RightTopNoteFadingIn -> RightTopNoteFadingIn
                        RightTopNoteFadingOut -> RightTopNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> RightTopNoteFadingOut
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote

xPressRightBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> RightBottomNoteFadingOut
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote 

xPressBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> BottomNoteFadingOut
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> StationaryLeftTopNote

xPressLeftBottom oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> LeftBottomNoteFadingOut
                        StationaryLeftTopNote -> StationaryLeftTopNote

xPressLeftTop oldState = case oldState of
                        Start -> Start
                        StationaryNoNote -> StationaryNoNote
                        RightBottomNoteFadingIn -> RightBottomNoteFadingIn
                        RightBottomNoteFadingOut -> RightBottomNoteFadingOut
                        TopNoteFadingIn -> TopNoteFadingIn
                        TopNoteFadingOut -> TopNoteFadingOut
                        RightTopNoteFadingIn -> RightBottomNoteFadingIn
                        RightTopNoteFadingOut -> RightBottomNoteFadingOut
                        BottomNoteFadingIn -> BottomNoteFadingIn
                        BottomNoteFadingOut -> BottomNoteFadingOut
                        LeftBottomNoteFadingIn -> LeftBottomNoteFadingIn
                        LeftBottomNoteFadingOut -> LeftBottomNoteFadingOut
                        LeftTopNoteFadingIn -> LeftTopNoteFadingIn
                        LeftTopNoteFadingOut -> LeftTopNoteFadingOut
                        StationaryRightTopNote -> StationaryRightTopNote
                        StationaryTopNote -> StationaryTopNote
                        StationaryRightBottomNote -> StationaryRightBottomNote
                        StationaryBottomNote -> StationaryBottomNote
                        StationaryLeftBottomNote -> StationaryLeftBottomNote
                        StationaryLeftTopNote -> LeftTopNoteFadingOut
                        
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
            -- Center Face
           , group [
                circle 11
                    |> filled backgroundPerson
                , face4
                , myCircleOutline
                    |> makeTransparent 0
                    
            ]
                |> move (0,-20)
            -- Top
            , group [
                circle 11
                    |> filled backgroundPerson
                , face1
                , myCircleOutline
                    |> makeTransparent model.topNoteTransparency
                    
            ]
                |> notifyTap ButtonPressTop
                |> move (0,(10))         --speed, % of the wheel
                |> scale (max(sin model.time*enlargedSize) scaledSize)
           
            -- Right Top
           , group [
                circle 11
                    |> filled backgroundPerson
                , face2
                , myCircleOutline
                    |> makeTransparent model.rightTopNoteTransparency
                    
            ]
                |> notifyTap ButtonPressRightTop
                |> move (26,(15-20))
                |> scale (max(sin (model.time-timeShift*1)*enlargedSize) scaledSize)
            -- Right Bottom
           , group [
                circle 11
                    |> filled backgroundPerson
                , face3
                , myCircleOutline
                    |> makeTransparent model.rightBottomNoteTransparency
                    
                    
            ]
                |> notifyTap ButtonPressRightBottom
                |> move (26,(-15-20))
                |> scale (max(sin (model.time-timeShift*2)*enlargedSize) scaledSize)
            -- Bottom
           , group [
                circle 11
                    |> filled backgroundPerson
                , face4
                , myCircleOutline
                    |> makeTransparent model.bottomNoteTransparency
                    
                    
            ]
                |> notifyTap ButtonPressBottom
                |> move (0,(-30-20))
                |> scale (max(sin (model.time-timeShift*3)*enlargedSize) scaledSize)
            -- Left Bottom
           , group [
                circle 11
                    |> filled backgroundPerson
                , face1
                , myCircleOutline
                    |> makeTransparent model.leftBottomNoteTransparency
                    
                    
            ]
                |> notifyTap ButtonPressLeftBottom
                |> move (-26,(-15-20))
                |> scale (max(sin (model.time-timeShift*4)*enlargedSize) scaledSize)
            -- Left Top
           , group [
                circle 11
                    |> filled backgroundPerson
                , face2
                , myCircleOutline
                    |> makeTransparent model.leftTopNoteTransparency
                    
                    
            ]
                |> notifyTap ButtonPressLeftTop
                |> move (-26,(15-20))
                |> scale (max(sin (model.time-timeShift*5)*enlargedSize) scaledSize)
           -- Top Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text1
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressTop
                ]
                    |> makeTransparent model.topNoteTransparency
                    |> move (model.topNoteX, model.topNoteY)
            -- Right Top Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text2
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressRightTop
                ]
                    |> makeTransparent model.rightTopNoteTransparency
                    |> move (model.rightTopNoteX, model.rightTopNoteY)
            -- Right Bottom Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text3
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressRightBottom
                ]
                    |> makeTransparent model.rightBottomNoteTransparency
                    |> move (model.rightBottomNoteX, model.rightBottomNoteY)
            -- Bottom Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text4
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressBottom
                ]
                    |> makeTransparent model.bottomNoteTransparency
                    |> move (model.bottomNoteX, model.bottomNoteY)
            -- Left Bottom Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text5
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressLeftBottom
                ]
                    |> makeTransparent model.leftBottomNoteTransparency
                    |> move (model.leftBottomNoteX, model.leftBottomNoteY)
            -- Left Top Note
           , group [
                roundedRect 80 20 5
                    |> filled noteColour
                    |> move (0, 35)
                , text text6
                    |> filled black
                    |> scale 0.45
                    |> move (-35 , 38)
                , closeBoxSymbol
                    |> rotate (degrees 45)
                    |> move (35, 40)
                    |> notifyTap XPressLeftTop
                ]
                    |> makeTransparent model.leftTopNoteTransparency
                    |> move (model.leftTopNoteX, model.leftTopNoteY)
            , text ("Animation Time " ++ (stringOfAniTime model.aniTimeFade))
                |> filled black
                |> move (30, -50)
            ]   -- END OF MODEL

-- DEFINING VARIABLES

positive x = if x > 0 then x else 0


text1 = "hello1"
text2 = "hello2"
text3 = "hello3"
text4 = "hello4"
text5 = "hello5"
text6 = "hello6"

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

myCircleOutline = circle 11
                    |> outlined (solid 1) orange

myCircle = group [circle 10
                         |> filled backgroundPerson
                -- Main body
                , curve (0,0) [Pull (5,10) (10,0)]
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


--BIYA'S WONDERFUL FACES

faceScale = 0.08

--CIRCLE OF 6 FACES 
face1 = group [
              --THIS IS THE WHOLE FACE
                    hair4b
                    --EARS
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin8
                         ] |> move (-70,0) 
                        
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin8
                         ] |> move (70,0)

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black 
                        |> move (0,-7) 
                   ,oval 140 153
                        |> filled skin8
                        |> move (0,-7)

                   ,freckles |> move (-40,-20)
                   ,freckles |> move (40,-20)

                  -- , blush |> move (-40,-20)
                   --, blush |> move (40,-20)

                   ,group [filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (rgb 75 114 72)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (rgb 75 114 72)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ]
                    ,hair4f
                        |> move(0,60)

                    ,curve (-50,-20) [Pull (0,-40) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (0,-40)
                      |> scale 0.4

                   ] |> scale faceScale --Scale the whole face 
                    |> move (0,1)

face2 = group [
              --THIS IS THE WHOLE FACE
                    hair3b
                    --EARS
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin3
                         ] |> move (-70,0) 
                        
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin3
                         ] |> move (70,0)

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black 
                        |> move (0,-7) 
                   ,oval 140 153
                        |> filled skin3
                        |> move (0,-7)

                   ,freckles |> move (-40,-20)
                   ,freckles |> move (40,-20)

                   --, blush |> move (-40,-20)
                   --, blush |> move (40,-20)

                   ,group [filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (rgb 161 202 241)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (rgb 161 202 241)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ]

                    ,hair3f
                        |> move(0,60)

                    ,curve (-50,-20) [Pull (0,-40) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (0,-40)
                      |> scale 0.4

                   ] |> scale faceScale --Scale the whole face 
                   |> move (0,1)

face3 = group [
              --THIS IS THE WHOLE FACE
                    hair2b
                    --EARS
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin9
                         ] |> move (-70,0) 
                        
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin9
                         ] |> move (70,0)

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black 
                        |> move (0,-7) 
                   ,oval 140 153
                        |> filled skin9
                        |> move (0,-7)

                   ,freckles |> move (-40,-20)
                   ,freckles |> move (40,-20)

                   --, blush |> move (-40,-20)
                   --, blush |> move (40,-20)

                   ,group [filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (rgb 75 114 72)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (rgb 75 114 72)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ]

                    ,hair2f
                        |> move(0,60)

                    ,curve (-50,-20) [Pull (0,-40) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (0,-40)
                      |> scale 0.4
                   ] |> scale faceScale --Scale the whole face 
                    |> move (0,1)

face4 = group [
              --THIS IS THE WHOLE FACE
                    hair1b
                    --EARS
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin7
                         ] |> move (-70,0) 
                        
                   ,group[
                       oval 24 44 |> filled black
                      ,oval 20 40 |> filled skin7
                         ] |> move (70,0)

                   --The oval that makes up the face
                   ,oval 143 156
                        |> filled black 
                        |> move (0,-7) 
                   ,oval 140 153
                        |> filled skin7
                        |> move (0,-7)

                   ,freckles |> move (-40,-20)
                   ,freckles |> move (40,-20)

                   --, blush |> move (-40,-20)
                   --, blush |> move (40,-20)

                   ,group [filled black (circle 20)|> move (-27,0) --Left eye
                          ,filled white (circle 18) |> move (-27,0)
                          ,circle 15
                              |> filled (rgb 79 56 3)
                              |> move (-24,0)
                          ,filled black (circle 12) |> move (-21,2)
                          ,filled white (circle 6) |> move (-18,3)
                          ,filled black (circle 20)|> move (27,0) --Right eye
                          ,filled white (circle 18) |> move (27,0)
                          ,circle 15
                              |> filled (rgb 79 56 3)
                              |> move (30,0)
                          ,filled black (circle 12) |> move (32,2)
                          ,filled white (circle 6) |> move (35,3)
                        ]
                    ,hair1f
                        |> move(0,60)
                    ,curve (-50,-20) [Pull (0,-40) (50, -20)]
                      |> outlined (solid 3) red
                      |> move (0,-40)
                      |> scale 0.4
                   ] |> scale faceScale --Scale the whole face
                 









--All of this code is the stuff that makes the face. Play around and make new faces 
--Use the Face1-4 templates to get started instead of building it from scratch
--Colours are at the very bottom
-----------------------------------------------------------------------------------
ear = group[
      oval 24 44 |> filled black
      ,oval 20 40 |> filled skin
    
     ]


eyebrow = group [ oval 37 4 |> filled black]     
     
blush = group [circle 15 |> filled pinkie]


--Curly dark hair
hair1f = group [ oval 60 40 |> filled black |> move(0,5)
              ,oval 60 40 |> filled black |> move (-30,0) |> rotate (degrees 20)
              ,oval 60 40 |> filled black |> move (30,0) |> rotate (degrees 160)
              ,oval 55 35 |> filled black |> move (55,-15) |> rotate (degrees 130)
              ,oval 55 35 |> filled black |> move (-55,-15) |> rotate (degrees 50)


              ]

hair1b = group []

--dreadlocks front
hair2f = group [ 
              oval 17 27 |> filled black |> move (-70,-35) |> rotate (degrees 60)
              ,oval 17 27 |> filled black |> move (70,-35) |> rotate (degrees -60)

              ,oval 12 22 |> filled hair2c |> move (-70,-35) |> rotate (degrees 60)
              ,oval 12 22 |> filled hair2c |> move (70,-35) |> rotate (degrees -60)
              
              ,oval 20 30 |> filled black |> move (65,-25) |> rotate (degrees -60)
              ,oval 15 25 |> filled hair2c |> move (65,-25) |> rotate (degrees -60)

              ,oval 20 30 |> filled black |> move (-65,-25) |> rotate (degrees 60)
              ,oval 15 25 |> filled hair2c |> move (-65,-25) |> rotate (degrees 60)

              ,oval 20 35 |> filled black |> move (-55,-15) |> rotate (degrees 50)
              ,oval 15 30 |> filled hair2c |> move (-55,-15) |> rotate (degrees 50)

              ,oval 20 35 |> filled black |> move (-45,-5) |> rotate (degrees 40)
              ,oval 15 30 |> filled hair2c |> move (-45,-5) |> rotate (degrees 40)

              ,oval 20 35 |> filled black |> move (55,-15) |> rotate (degrees 130)
              ,oval 15 30 |> filled hair2c |> move (55,-15) |> rotate (degrees 130)

              ,oval 20 35 |> filled black |> move (45,-5) |> rotate (degrees -40)
              ,oval 15 30 |> filled hair2c |> move (45,-5) |> rotate (degrees -40)

              ,oval 20 40 |> filled black |> move (30,0) |> rotate (degrees 160)
              ,oval 15 35 |> filled hair2c |> move (30,0) |> rotate (degrees 160)

              ,oval 20 40 |> filled black |> move (-30,0) |> rotate (degrees 20)
              ,oval 15 35 |> filled hair2c |> move (-30,0) |> rotate (degrees 20)

              ,oval 20 40 |> filled black |> move(-15,5) |> rotate (degrees 15)
              ,oval 15 35 |> filled hair2c |> move(-15,5) |> rotate (degrees 15)
             
             ,oval 20 40 |> filled black |> move(15,5) |> rotate (degrees -15) 
             ,oval 15 35 |> filled hair2c |> move(15,5) |> rotate (degrees -15) 
             ,oval 23 43 |> filled black |> move(0,7)
             ,oval 18 38 |> filled hair2c |> move(0,7)
                ]

--dreadlocks back
hair2b = group[--Long hair
          roundedRect 18 75 10 |> filled black |> move (-70,-50)
          ,roundedRect 13 70 10 |> filled hair2c |> move (-70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (-55,-65)
          ,roundedRect 13 70 10 |> filled hair2c |> move (-55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (70,-50)
          ,roundedRect 13 70 10 |> filled hair2c |> move (70,-50)

          ,roundedRect 18 75 10 |> filled black |> move (55,-65)
          ,roundedRect 13 70 10 |> filled hair2c |> move (55,-65)

          ,roundedRect 18 75 10 |> filled black |> move (-38,-75) |> rotate (degrees -5)
          ,roundedRect 13 70 10 |> filled hair2c |> move (-38,-75) |> rotate (degrees -5)

          ,roundedRect 18 75 10 |> filled black |> move (38,-75) |> rotate (degrees 5)
          ,roundedRect 13 70 10 |> filled hair2c |> move (38,-75) |> rotate (degrees 5)

          ]

--bangs front
hair3f = group [  bangsl
                    |> scale 1.2
                    |> move (11,10)
                   ,bangsl
                    |> scale 1.2
                    |> move (-12,10)
                    |> mirrorX

                   ,circle 5 
                    |> filled brown1
                    |> move(0,23.2)
                     --highlights
                   ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 1) highlights
                    |> move (-75,-65)
                    |> scale 1.6 
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 1) highlights
                    |> move (-72,-55)
                    |> scale 1.3

                    ,curve (0,0) [Pull (50,10) (40,50)]
                    |> outlined (solid 1) highlights
                    |> move (75,-65)
                    |> scale 1.6 
                    |> mirrorX
                    ,curve (0,0) [Pull (40,10) (40,50)]
                    |> outlined (solid 1) highlights
                    |> move (72,-55)
                    |> scale 1.3
                    |> mirrorX
               ]
--Bangs back // just long brown hair
hair3b = group [ 
               roundedRect 145 140 30 
                    |> filled brown1 
                    |> move (0,-50) 
                , rect 2 140 
                    |> filled highlights
                    |> move (-60,-30)
                , rect 2 150 
                    |> filled highlights
                    |> move (-50,-40)
                , rect 2 150 
                    |> filled highlights
                    |> move (-30,-40)
                , rect 2 140 
                    |> filled highlights
                    |> move (60,-30)
                , rect 2 150 
                    |> filled highlights
                    |> move (50,-40)
                , rect 2 150 
                    |> filled highlights
                    |> move (30,-40)
                , rect 30 150 
                    |> filled backgroundPerson
                    |> move (0,-50)


               ]
--Part of hair3b
bangsl = group [
                curve (0,0) [Pull (10,50) (40,50)]
                    |> filled brown1 
                    |> move (-75,-65)
                    |> scale 1.6
                ,curve (0,0) [Pull (50,10) (40,50)]
                    |> filled brown1
                    |> move (-75,-65)
                    |> scale 1.6  
                    ]

--Curly red hair front
hair4f = group [ 
                oval 60 95
                      |> filled black
                      |> rotate (degrees -65)
                      |> move (-20,-15)
                ,oval 20 30 
                      |> filled black
                      |> rotate (degrees 60)
                      |> move (27,0)
                ,oval 60 95 
                      |> filled redhair
                      |> rotate (degrees -65)
                      |> move (-20,-13)
                ,oval 20 30 
                      |> filled redhair
                      |> rotate (degrees 60)
                      |> move (27,2)

                  ]
--Curly red hair back
hair4b = group [
            -- top of the head
            oval 154 74 
                |> filled black
                |> move (0,55)
            ,oval 54 74 
                |> filled black
                |> move (55,40)
                |> rotate (degrees -40)
            ,oval 54 74
                |> filled black
                |> move (-55,40)
                |> rotate (degrees 40)
            ,oval 74 184
                |> filled black
                |> rotate (degrees 5) 
                |> move (40,-15)
            ,oval 44 74
                |> filled black
                |> move (80,10)
                |> rotate (degrees 5)
            ,oval 74 184 
                |> filled black
                |> rotate (degrees -5) 
                |> move (-40,-15)
            ,oval 44 74 
                |> filled black
                |> move (-80,10)
                |> rotate (degrees -5)
            ,oval 74 184 
                |> filled black
                |> rotate (degrees -25) 
                |> move (-50,0)
            --long 
           ,oval 74 184
                |> filled black
                |> rotate (degrees 25) 
                |> move (50,0)
           ,oval 150 70 
                |> filled redhair 
                |> move (0,55)
           ,oval 70 180 
                |> filled redhair 
                |> rotate (degrees 25) 
                |> move (50,0)
            --long
            ,oval 70 180 
                |> filled redhair 
                |> rotate (degrees 5) 
                |> move (40,-15)
            --long 
            ,oval 40 70 
                |> filled redhair
                |> move (-80,10)
                |> rotate (degrees -5)
           ,oval 70 180 
                |> filled redhair 
                |> rotate (degrees -25) 
                |> move (-50,0)
            --long
            ,oval 70 180 
                |> filled redhair 
                |> rotate (degrees -5) 
                |> move (-40,-15)
            --side head
            ,oval 50 70 
                |> filled redhair
                |> move (55,40)
                |> rotate (degrees -40)  
            --side head
            ,oval 50 70
                |> filled redhair
                |> move (-55,40)
                |> rotate (degrees 40) 
            ,oval 40 70 
                |> filled redhair
                |> move (80,10)
                |> rotate (degrees 5)

                ]                   

freckles = group [
            circle 1.3 |> filled brown1
            ,circle 1.3 |> filled brown1 |> move (-4,-4)
            ,circle 1.3 |> filled brown1 |> move (4,-4)]


-- COLOURS 
skin = skin10

skin1 = rgb 255 223 196

skin2 = rgb 238 206 179

skin3 = rgb 225 170 100

skin4 = rgb 229 184 143

skin5 = rgb 231 158 109

skin6 = rgb 219 144 101

skin7 = rgb 186 108 73 

skin8 = rgb 198 120 86  

skin9 = rgb 130 72 24 -- 

skin10 = rgb 146 106 45

skin11 = rgb 135 97 39

skin12 = rgb 124 80 26 

skin13 = rgb 126 77 28 

skin14 = rgb 137 78 26

skin15 = rgb 88 53 18 

skin16 = rgb 62 43 19

lips = 
  rgb 150 18 18
  

lbrown = rgb 79 56 3

redhair = rgb 67 0 0
redhair2 = rgb 60 0 0

brown1 = rgb 61 27 0
highlights = rgb 89 40 0

pinkie = rgba 255 158 158 0.2

hair2c = rgb 40 40 40









