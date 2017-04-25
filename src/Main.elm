module Main exposing (main)

import AnimationFrame
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html
import Html.Attributes
import Html.Events
import List exposing (..)
import Mouse
import Task
import Time exposing (..)
import Window


-- import Signal exposing (..)

import Mouse
import Window


type alias Complex =
    ( Float, Float )


addC : ( number, number1 ) -> ( number, number1 ) -> ( number, number1 )
addC ( r1, i1 ) ( r2, i2 ) =
    ( r1 + r2, i1 + i2 )


subC : ( number, number1 ) -> ( number, number1 ) -> ( number, number1 )
subC ( r1, i1 ) ( r2, i2 ) =
    ( r1 - r2, i1 - i2 )


mulC : ( number, number ) -> ( number, number ) -> ( number, number )
mulC ( r1, i1 ) ( r2, i2 ) =
    ( r1 * r2 - i1 * i2, i1 * r2 + r1 * i2 )


absC : ( Float, Float ) -> Float
absC ( r1, i1 ) =
    sqrt (r1 * r1 + i1 * i1)


distC : ( Float, Float ) -> ( Float, Float ) -> Float
distC x y =
    absC (subC x y)


scaleC : number -> ( number, number ) -> ( number, number )
scaleC a ( r, i ) =
    ( r * a, i * a )


normalizeC : ( Float, Float ) -> ( Float, Float )
normalizeC c =
    scaleC (1 / absC c) c


rotateLeftC : ( a, number ) -> ( number, a )
rotateLeftC ( r, i ) =
    ( -i, r )


liveTimeFactorInside : Float
liveTimeFactorInside =
    2 / velocity


liveTimeFactorOutside : Float
liveTimeFactorOutside =
    0.3 / velocity


maxItersPreview : number
maxItersPreview =
    10


maxItersSelection : Int
maxItersSelection =
    50


maxPoints : Int
maxPoints =
    240


velocity : Float
velocity =
    1.8 / second


type alias MandelTrace =
    List Complex


type alias MLine =
    { from : Complex
    , to : Complex
    , distance : Float
    , length : Float
    }


toMLine : ( Complex, Complex ) -> Float -> MLine
toMLine ( f, t ) d =
    { from = f
    , to = t
    , distance = d
    , length = absC (subC f t)
    }


toMLines : MandelTrace -> ( Float, List MLine )
toMLines points =
    let
        ls =
            List.map2 (,) points (drop 1 points)

        toMLine_ l ( distance, mlines ) =
            let
                newMLine =
                    toMLine l distance
            in
                ( distance + newMLine.length, newMLine :: mlines )
    in
        foldl toMLine_ ( 0, [] ) ls


type alias AnimatedTrace =
    { lines : List MLine
    , c : Complex
    , zlast : Complex
    , age : Time
    , ttl : Time
    , inside : Bool
    , color : ( Float, Float, Float )
    }


type alias St =
    { traces : List AnimatedTrace
    , zoom : Float
    , offset : Complex
    , r_range : Complex
    , i_range : Complex
    , preview : ( Float, MandelTrace )
    , previewActive : Bool
    , ds : DeviceState
    }


initialSt : ( St, Cmd UserInput )
initialSt =
    ( { traces = []
      , zoom = 1
      , offset = ( 0, 0 )
      , r_range = ( -2, 2 )
      , i_range = ( -2, 2 )
      , preview = ( 0.0, [] )
      , previewActive = False
      , ds = deviceStateFromScreen { width = 100, height = 100 }
      }
    , Task.perform WindowResize Window.size
    )


zoomTo : Float -> St -> St
zoomTo zoom st =
    { st
        | zoom = zoom
        , r_range = ( -2 / zoom, 2 / zoom )
        , i_range = ( -2 / zoom, 2 / zoom )
    }


type alias DeviceState =
    { screen_w_range : ( Float, Float )
    , screen_w : Int
    , screen_h_range : ( Float, Float )
    , screen_h : Int
    , mouse_w_range : ( Float, Float )
    , mouse_h_range : ( Float, Float )
    , screen_diag : Float
    }


deviceStateFromScreen : Window.Size -> DeviceState
deviceStateFromScreen ws =
    let
        w =
            ws.width

        h =
            ws.height

        ( aw, ah ) =
            if w > h then
                ( h, h )
            else
                ( w, w )

        w_half =
            toFloat aw / 2

        h_half =
            toFloat ah / 2

        mouse_w_range =
            ( toFloat (w - aw) / 2, toFloat (w - aw) / 2 + toFloat aw )

        mouse_h_range =
            ( toFloat (h - ah) / 2, toFloat (h - ah) / 2 + toFloat ah )
    in
        { screen_w_range = ( -w_half, w_half )
        , screen_w = w
        , screen_h_range = ( h_half, -h_half )
        , screen_h = h
        , mouse_w_range = mouse_w_range
        , mouse_h_range = mouse_h_range
        , screen_diag = absC (addC mouse_w_range mouse_h_range)
        }


outOfMandelbrot : Int -> Complex -> MandelTrace
outOfMandelbrot itersMax c =
    let
        outOfMandelbrotAcc iters z zs =
            if absC z >= 2.0 || iters == 0 then
                zs
            else
                let
                    z_ =
                        addC (mulC z z) c
                in
                    outOfMandelbrotAcc (iters - 1) z_ (z_ :: zs)
    in
        outOfMandelbrotAcc itersMax c [ c ]


scale : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
scale ( fromMin, fromMax ) ( toMin, toMax ) from =
    (from - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin


zoom : Float -> ( Float, Float ) -> Complex -> Complex
zoom z ( offset_x, offset_y ) ( r, i ) =
    ( r * z + offset_x, i * z + offset_y )


mouseToMandel : Mouse.Position -> St -> Complex
mouseToMandel mouse st =
    ( scale st.ds.mouse_w_range st.r_range (toFloat mouse.x)
    , scale st.ds.mouse_h_range st.i_range (toFloat mouse.y)
    )


mandelToScreen : St -> Complex -> ( Float, Float )
mandelToScreen st ( r, i ) =
    ( scale st.r_range st.ds.screen_w_range r
    , scale st.i_range st.ds.screen_h_range i
    )



-- CALCULATIONS


update : UserInput -> St -> ( St, Cmd UserInput )
update ui st =
    let
        st_ =
            case ui of
                ZoomIn ->
                    zoomTo (st.zoom * 1.2) st

                ZoomReset ->
                    zoomTo 1.0 st

                ZoomOut ->
                    zoomTo (max 0.75 (st.zoom / 1.2)) st

                WindowResize window ->
                    { st | ds = deviceStateFromScreen window }

                EnablePreview mouse ->
                    preview mouse { st | previewActive = True }

                Preview mouse ->
                    if st.previewActive then
                        preview mouse st
                    else
                        st

                Select mouse ->
                    select mouse
                        { st
                            | previewActive = False
                            , preview = ( 0.0, [] )
                        }

                Animate delta ->
                    animate delta st
    in
        ( st_, Cmd.none )


preview : Mouse.Position -> St -> St
preview mouse st =
    let
        start =
            mouseToMandel mouse st
    in
        { st
            | preview =
                ( getBaseColor start st.r_range
                , outOfMandelbrot maxItersPreview start
                )
        }


totalPoints : St -> Int
totalPoints st =
    List.foldr
        (\t s -> s + List.length t.lines)
        0
        st.traces


timeToNextTraceDeath : St -> Float
timeToNextTraceDeath st =
    List.foldr
        (\t d ->
            let
                d_ =
                    t.ttl - t.age
            in
                if d <= 0.0 then
                    d_
                else if d_ <= 0.0 then
                    d
                else
                    min d d_
        )
        0.0
        st.traces


select : Mouse.Position -> St -> St
select mouse st =
    let
        selection =
            mouseToMandel mouse st

        maxIters =
            min maxItersSelection (maxPoints - totalPoints st)

        traceRev =
            outOfMandelbrot maxIters selection

        trace =
            reverse traceRev

        ( totalLength, mlines ) =
            toMLines trace
    in
        case List.reverse mlines of
            [] ->
                st

            lastMLine :: _ ->
                case traceRev of
                    [] ->
                        st

                    lastTrace :: _ ->
                        let
                            inside =
                                absC lastTrace <= 2.0

                            mlinesLength =
                                sum << List.map (\l -> l.length)

                            ttl =
                                (if inside then
                                    liveTimeFactorInside
                                 else
                                    liveTimeFactorOutside
                                )
                                    * mlinesLength mlines

                            sat =
                                scale ( 0, toFloat maxItersSelection )
                                    ( 0.3, 0.8 )
                                    (toFloat (length mlines))

                            animatedTrace =
                                { lines = mlines
                                , ttl = ttl
                                , age = 0
                                , c = selection
                                , zlast = lastTrace
                                , inside = inside
                                , color = ( getBaseColor selection st.r_range, sat, 0.5 )
                                }
                        in
                            { st
                                | traces =
                                    animatedTrace :: st.traces
                            }


getBaseColor : ( Float, Float ) -> ( Float, Float ) -> Float
getBaseColor z r_range =
    let
        hue =
            scale r_range ( 400, 1500 ) (absC z)
    in
        degrees hue


animate : Time -> St -> St
animate dt st =
    let
        updateAge st =
            let
                increaseAge dt t =
                    { t | age = t.age + dt }
            in
                { st | traces = List.map (increaseAge dt) st.traces }

        removeOld st =
            let
                isAlive t =
                    t.ttl > t.age
            in
                { st | traces = List.filter isAlive st.traces }
    in
        removeOld (updateAge st)



-- RENDERING


render : St -> Html.Html UserInput
render st =
    Html.div []
        [ Html.div
            [ Html.Attributes.style
                [ ( "position", "absolute" )
                , ( "color", "gray" )
                , ( "z-index", "2" )
                ]
            ]
            [ Html.table []
                [ Html.tr []
                    [ Html.td []
                        [ Html.text "Total Points" ]
                    , Html.td
                        []
                        [ Html.text
                            (toString (totalPoints st)
                                ++ "/"
                                ++ toString maxPoints
                            )
                        ]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ Html.text " Time till next cleanup" ]
                    , Html.td
                        []
                        [ Html.text
                            (toString (round (timeToNextTraceDeath st)))
                        ]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ Html.text "Zoom" ]
                    , Html.td
                        []
                        [ Html.text (toString st.zoom)
                        ]
                    ]
                ]
            , Html.div []
                [ Html.button [ Html.Events.onClick ZoomIn ] [ Html.text "+" ]
                , Html.button [ Html.Events.onClick ZoomReset ] [ Html.text "1.0" ]
                , Html.button [ Html.Events.onClick ZoomOut ] [ Html.text "-" ]
                ]
            ]
        , Element.toHtml <|
            let
                diagrams =
                    collage st.ds.screen_w
                        st.ds.screen_h
                        (renderPreview st ++ renderSelection st)

                --show st
                minScreenRange =
                    if st.ds.screen_w < st.ds.screen_h then
                        st.ds.screen_w_range
                    else
                        st.ds.screen_h_range

                domainCircle =
                    collage st.ds.screen_w
                        st.ds.screen_h
                        [ filled black <|
                            circle <|
                                scale ( 0, 1 / st.zoom ) minScreenRange 1.0
                        ]
            in
                layers
                    [ domainCircle
                    , diagrams
                    ]
        ]


renderPreview : St -> List Form
renderPreview st =
    let
        ( hue, zs ) =
            st.preview

        lineStyle =
            dashed (Color.hsla hue 0.7 0.7 0.5)
    in
        [ traced lineStyle <| path <| List.map (mandelToScreen st) zs ]


renderSelection : St -> List Form
renderSelection st =
    concatMap (renderAnimatedMandelTrace st) st.traces


renderAnimatedMandelTrace : St -> AnimatedTrace -> List Form
renderAnimatedMandelTrace st t =
    concatMap (renderMLine st t.age t.inside t.color) t.lines


renderMLine : St -> Float -> Bool -> ( Float, Float, Float ) -> MLine -> List Form
renderMLine st age insideMandelbrot ( hue, sat, lum ) l =
    let
        glow =
            (age * velocity - l.distance) / l.length

        glowing =
            glow > 0 && glow < 1

        warmingUp =
            glow <= 0

        coolingDown =
            glow >= 1 && glow < 5

        cool =
            glow >= 5

        screenFrom =
            mandelToScreen st l.from

        screenTo =
            mandelToScreen st l.to

        screenLength =
            distC screenTo screenFrom

        heat =
            if glowing then
                1
            else if coolingDown || cool then
                1 / logBase 2 (glow + 1)
            else if warmingUp then
                -1 / (glow - 1)
            else
                0

        lineColor =
            let
                intensity =
                    if warmingUp then
                        0.8 * heat + 0.2
                    else if (coolingDown || cool) && glow <= 2 then
                        heat
                    else
                        heat
            in
                hsla hue sat lum intensity

        drawLine c f t =
            traced (solid c) <| path [ f, t ]

        drawCircle style radius =
            let
                pos =
                    addC screenFrom (scaleC glow (subC screenTo screenFrom))
            in
                move pos <| style <| circle radius

        drawMarkerCircle =
            let
                radius =
                    st.ds.screen_diag / 350

                style =
                    filled lineColor
            in
                drawCircle style radius

        drawMarkerCircleShadow =
            if insideMandelbrot then
                let
                    radius =
                        if glowing then
                            screenLength / 4 * heat
                        else if warmingUp then
                            0
                        else
                            screenLength / 4

                    alpha =
                        0.4 - 0.2 * heat

                    style =
                        filled <| hsla hue sat lum alpha
                in
                    [ drawCircle style radius ]
            else
                []
    in
        if warmingUp then
            [ drawLine lineColor screenFrom screenTo ]
        else if glowing then
            let
                shadowDistance =
                    0.01 * l.length * glow * st.ds.screen_diag

                ortho =
                    scaleC shadowDistance <| normalizeC <| rotateLeftC <| subC screenTo screenFrom

                fromShadowAbove =
                    addC screenFrom ortho

                toShadowAbove =
                    addC screenTo ortho

                fromShadowBelow =
                    subC screenFrom ortho

                toShadowBelow =
                    subC screenTo ortho

                shadowColor =
                    hsla hue
                        (sat / 2)
                        lum
                        (0.75 - 0.75 * glow)
            in
                [ drawLine lineColor screenFrom screenTo
                , drawLine shadowColor fromShadowAbove toShadowAbove
                , drawLine shadowColor fromShadowBelow toShadowBelow
                , drawMarkerCircle
                ]
        else if coolingDown then
            [ drawLine lineColor screenFrom screenTo
            ]
                ++ drawMarkerCircleShadow
        else if cool then
            [ drawLine lineColor screenFrom screenTo
            ]
                ++ drawMarkerCircleShadow
        else
            []



-- INPUT


type UserInput
    = Preview Mouse.Position
    | EnablePreview Mouse.Position
    | Select Mouse.Position
    | Animate Time
    | WindowResize Window.Size
    | ZoomIn
    | ZoomReset
    | ZoomOut


subscriptions : St -> Sub UserInput
subscriptions _ =
    Sub.batch
        [ Mouse.ups Select
        , Mouse.moves Preview
        , Mouse.downs EnablePreview
        , Window.resizes WindowResize
        , AnimationFrame.diffs Animate
        ]



-- MAIN LOOP


main : Program Never St UserInput
main =
    Html.program
        { init = initialSt
        , update = update
        , subscriptions = subscriptions
        , view = render
        }
