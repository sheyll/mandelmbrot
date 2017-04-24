module Main exposing (main)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html
import Task
import List exposing (..)
import Time exposing (..)
import AnimationFrame


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


maxItersPreview : number
maxItersPreview =
    4


maxItersSelection : number
maxItersSelection =
    40


maxSelection : number
maxSelection =
    12


velocity : Float
velocity =
    3 / second


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


mlinesLength : List MLine -> Float
mlinesLength =
    sum << List.map (\l -> l.length)


type alias AnimatedTrace =
    { lines : List MLine
    , c : Complex
    , zlast : Complex
    , age : Time
    , ttl : Time
    }


type alias St =
    { traces : List AnimatedTrace
    , zoom : Float
    , offset : Complex
    , r_range : Complex
    , i_range : Complex
    , preview : MandelTrace
    , ds : DeviceState
    }


initialSt : ( St, Cmd UserInput )
initialSt =
    ( { traces = []
      , zoom = 1.0
      , offset = ( 0, 0 )
      , r_range = ( -2, 2 )
      , i_range = ( -2, 2 )
      , preview = []
      , ds = deviceStateFromScreen { width = 100, height = 100 }
      }
    , Task.perform WindowResize Window.size
    )


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


outOfMandelbrot : Int -> Complex -> MandelTrace -> MandelTrace
outOfMandelbrot iters c zs =
    case zs of
        [] ->
            []

        z :: _ ->
            if absC z >= 2.0 then
                zs
            else if iters == 0 then
                zs
            else
                let
                    z_ =
                        addC (mulC z z) c
                in
                    outOfMandelbrot (iters - 1) c (z_ :: zs)


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
                WindowResize window ->
                    { st | ds = deviceStateFromScreen window }

                Preview mouse ->
                    preview mouse st

                Select mouse ->
                    select mouse st

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
        { st | preview = outOfMandelbrot maxItersPreview start [ start ] }


select : Mouse.Position -> St -> St
select mouse st =
    let
        selection =
            mouseToMandel mouse st

        traceRev =
            outOfMandelbrot maxItersSelection selection [ selection ]

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

                            ttlFactor =
                                if inside then
                                    10
                                else
                                    1

                            ttl =
                                (totalLength * ttlFactor) / velocity

                            animatedTrace =
                                { lines = mlines
                                , ttl = ttl
                                , age = 0
                                , c = selection
                                , zlast = lastTrace
                                }
                        in
                            { st
                                | traces = (animatedTrace :: take (maxSelection - 1) st.traces)
                                , preview = []
                            }


animate : Time -> St -> St
animate dt st =
    let
        age_updated st =
            { st | traces = List.map (increaseAge dt) st.traces }

        increaseAge dt t =
            { t | age = t.age + dt }

        old_removed st =
            { st | traces = List.filter hasTTL st.traces }

        hasTTL t =
            t.ttl > t.age
    in
        old_removed (age_updated st)



-- RENDERING


render : St -> Html.Html UserInput
render st =
    Element.toHtml <|
        let
            diagrams =
                collage st.ds.screen_w
                    st.ds.screen_h
                    (renderPreview st ++ renderSelection st)

            debugInfo =
                show st

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

                -- , debugInfo
                , diagrams
                ]


renderPreview : St -> List Form
renderPreview st =
    let
        zs =
            st.preview

        lineStyle =
            dashed charcoal
    in
        [ traced lineStyle <| path <| List.map (mandelToScreen st) zs ]


renderSelection : St -> List Form
renderSelection st =
    concatMap (renderAnimatedMandelTrace st) st.traces


renderAnimatedMandelTrace : St -> AnimatedTrace -> List Form
renderAnimatedMandelTrace st t =
    let
        ( cr, ci ) =
            t.c

        hue =
            scale st.r_range ( 0, 720 ) (absC t.zlast)

        sat =
            scale ( 0, toFloat maxItersSelection ) ( 0.1, 0.7 ) (toFloat (List.length t.lines))

        baseColor =
            ( degrees hue, sat, 0.5 )
    in
        concatMap (renderMLine st t.age baseColor) t.lines


renderMLine : St -> Float -> ( Float, Float, Float ) -> MLine -> List Form
renderMLine st age ( hue, sat, lum ) l =
    let
        animDist =
            age * velocity

        glow =
            (animDist - l.distance) / l.length

        glowing =
            glow > 0 && glow < 1

        warmingUp =
            glow <= 0

        coolingDown =
            glow >= 1 && glow < 10

        cool =
            glow >= 10

        from_ =
            mandelToScreen st l.from

        to_ =
            mandelToScreen st l.to

        length_ =
            distC to_ from_

        glowHill =
            let
                x =
                    2 * glow - 1
            in
                1 - (x * x)

        glowLog2 =
            logBase 2 (glow + 1)

        currentSat =
            if glowing then
                glowHill * (1 - sat) + sat
            else
                sat

        currentColor =
            hsla hue currentSat lum <|
                if glowing then
                    1
                else if coolingDown then
                    1 / glow
                else if cool then
                    0.2
                else if warmingUp then
                    0.5
                else
                    0

        drawLine c f t =
            traced (solid c) <| path [ f, t ]

        drawCircle style radius =
            let
                pos =
                    addC from_ (scaleC glow (subC to_ from_))
            in
                move pos <| style <| circle radius

        drawMarkerCircle =
            let
                radius =
                    length_ / st.ds.screen_diag * 30

                style =
                    filled currentColor
            in
                drawCircle style radius

        drawMarkerCircleShadow =
            let
                radius =
                    if glowing then
                        length_ / 4 * glowLog2
                    else if warmingUp then
                        0
                    else
                        length_ / 4

                alpha =
                    if cool then
                        1 / glow
                    else if warmingUp then
                        0.1
                    else
                        0.2

                style =
                    filled <| hsla hue sat lum 0.2
            in
                drawCircle style radius
    in
        if warmingUp then
            [ drawLine currentColor from_ to_ ]
        else if glowing then
            let
                shadowDistance =
                    0.01 * glowLog2 * st.ds.screen_diag

                ortho =
                    scaleC shadowDistance <| normalizeC <| rotateLeftC <| subC to_ from_

                fromShadowAbove =
                    addC from_ ortho

                toShadowAbove =
                    addC to_ ortho

                fromShadowBelow =
                    subC from_ ortho

                toShadowBelow =
                    subC to_ ortho

                shadowColor =
                    hsla hue (sat / 2) lum (0.5 - 0.5 * glowLog2)
            in
                [ drawLine currentColor from_ to_
                , drawLine shadowColor fromShadowAbove toShadowAbove
                , drawLine shadowColor fromShadowBelow toShadowBelow
                , drawMarkerCircle
                , drawMarkerCircleShadow
                ]
        else if coolingDown then
            [ drawLine currentColor from_ to_
            , drawMarkerCircleShadow
            ]
        else if cool then
            [ drawLine currentColor from_ to_ ]
        else
            []



-- INPUT


type UserInput
    = Preview Mouse.Position
    | Select Mouse.Position
    | Animate Time
    | WindowResize Window.Size


subscriptions : St -> Sub UserInput
subscriptions _ =
    Sub.batch
        [ Mouse.clicks Select
        , Mouse.moves Preview
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



-- main =
--     Signal.map2 render deviceState <| foldp update initialSt input
