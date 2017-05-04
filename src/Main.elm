module Main exposing (main)

import AnimationFrame
import Html
import Mouse
import Task
import Window
import Html.Attributes
import Html.Events
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import List exposing (..)
import Time exposing (..)


-- MATH


type alias Complex =
    ( Float, Float )


type alias Path =
    List Complex


type alias Line =
    ( Complex, Complex )


addC : Complex -> Complex -> Complex
addC ( r1, i1 ) ( r2, i2 ) =
    ( r1 + r2, i1 + i2 )


subC : Complex -> Complex -> Complex
subC ( r1, i1 ) ( r2, i2 ) =
    ( r1 - r2, i1 - i2 )


mulC : Complex -> Complex -> Complex
mulC ( r1, i1 ) ( r2, i2 ) =
    ( r1 * r2 - i1 * i2, i1 * r2 + r1 * i2 )


absC : Complex -> Float
absC ( r1, i1 ) =
    sqrt (r1 * r1 + i1 * i1)


distC : Complex -> Complex -> Float
distC x y =
    absC (subC x y)


scaleC : Float -> Complex -> Complex
scaleC a ( r, i ) =
    ( r * a, i * a )


normalizeC : Complex -> Complex
normalizeC c =
    scaleC (1 / absC c) c


rotateLeftC : Complex -> Complex
rotateLeftC ( r, i ) =
    ( -i, r )


scale : Complex -> Complex -> Float -> Float
scale ( fromMin, fromMax ) ( toMin, toMax ) from =
    (from - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin


zoom : Float -> Complex -> Complex -> Complex
zoom z ( offset_x, offset_y ) ( r, i ) =
    ( r * z + offset_x, i * z + offset_y )



-- MANDELBROT FRACTAL MATH


type alias MandelbrotDomain =
    { zoom : Float
    , offset : Complex
    , r_range : Complex
    , i_range : Complex
    }


defaultMandelbrotDomain : MandelbrotDomain
defaultMandelbrotDomain =
    { zoom = 1.0
    , offset = ( 0.0, 0.0 )
    , r_range = ( -2.0, 2.0 )
    , i_range = ( -2.0, 2.0 )
    }


zoomMandelbrotDomain : Float -> MandelbrotDomain -> MandelbrotDomain
zoomMandelbrotDomain z md =
    let
        zSafe =
            max 0.1 z

        zr =
            1 / zSafe
    in
        { zoom = zSafe
        , offset = md.offset
        , r_range = scaleC zr defaultMandelbrotDomain.r_range
        , i_range = scaleC zr defaultMandelbrotDomain.i_range
        }


type alias Orbital =
    { length : Float
    , segments : List OrbitalSegment
    , c : Complex
    , zlast : Complex
    , inside : Bool
    }


type alias OrbitalSegment =
    { from : Complex
    , to : Complex
    , distance : Float
    , length : Float
    }


segmentsToPath : List OrbitalSegment -> Path
segmentsToPath ss =
    case ss of
        [] ->
            []

        first :: rest ->
            List.map .to
                ({ first | to = first.from } :: first :: rest)


emptyOrbital : Orbital
emptyOrbital =
    { length = 0
    , segments = []
    , c = ( 0, 0 )
    , zlast = ( 0, 0 )
    , inside = True
    }


mandelbrotOrbital : Int -> Complex -> Orbital
mandelbrotOrbital itersMax c =
    let
        reversedPath =
            let
                pathLoop iters z zs =
                    if absC z >= 2.0 || iters == 0 then
                        zs
                    else
                        let
                            z_ =
                                addC (mulC z z) c
                        in
                            pathLoop (iters - 1) z_ (z_ :: zs)
            in
                pathLoop itersMax c [ c ]

        path =
            reverse reversedPath

        lines =
            List.map2 (,) path (drop 1 path)

        zlast =
            Maybe.withDefault c (List.head reversedPath)

        inside =
            absC zlast < 2.0

        addSegment ( f, t ) orbital =
            let
                newOrbitalSegment =
                    { from = f
                    , to = t
                    , distance = orbital.length
                    , length = absC (subC t f)
                    }

                newLength =
                    orbital.length + newOrbitalSegment.length

                newSegments =
                    newOrbitalSegment :: orbital.segments
            in
                { orbital
                    | length = newLength
                    , segments = newSegments
                }
    in
        foldl addSegment
            { emptyOrbital
                | c = c
                , zlast = zlast
                , inside = inside
            }
            lines



-- SCREEN FROM/TO MANDELBROT DOMAIN PROJECTION


type alias ScreenDimensions =
    { screen_w_range : Complex
    , screen_w : Int
    , screen_h_range : Complex
    , screen_h : Int
    , mouse_w_range : Complex
    , mouse_h_range : Complex
    , screen_diag : Float
    }


makeScreenDimensions : Window.Size -> ScreenDimensions
makeScreenDimensions ws =
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


projectIntoMandelbrot : Mouse.Position -> ScreenDimensions -> MandelbrotDomain -> Complex
projectIntoMandelbrot mouse sd md =
    ( scale sd.mouse_w_range md.r_range (toFloat mouse.x)
    , scale sd.mouse_h_range md.i_range (toFloat mouse.y)
    )


projectToScreen : Complex -> MandelbrotDomain -> ScreenDimensions -> Complex
projectToScreen ( r, i ) md sd =
    ( scale md.r_range sd.screen_w_range r
    , scale md.i_range sd.screen_h_range i
    )



-- MODEL


type alias Model =
    { traces : List AnimatedOrbital
    , preview : Maybe OrbitalPreview
    , md : MandelbrotDomain
    , ds : ScreenDimensions
    , splashScreenAlpha : Maybe Float
    , controls : Bool
    }


type alias OrbitalPreview =
    { baseColor : Float
    , orbital : Orbital
    }


type alias AnimatedOrbital =
    { orbital : Orbital
    , age : Time
    , ttl : Time
    , color : ( Float, Float, Float )
    }


init : ( Model, Cmd Msg )
init =
    ( { traces = []
      , preview = Nothing
      , md = zoomMandelbrotDomain defaultZoom defaultMandelbrotDomain
      , ds = makeScreenDimensions { width = 100, height = 100 }
      , splashScreenAlpha = Just 1.0
      , controls = False
      }
    , Task.perform WindowResize Window.size
    )



-- UPDATE


type Msg
    = UpdatePreview Mouse.Position
    | EnablePreview Mouse.Position
    | Select Mouse.Position
    | Animate Time
    | WindowResize Window.Size
    | ZoomIn
    | ZoomReset
    | ZoomOut
    | ToggleControls


update : Msg -> Model -> ( Model, Cmd Msg )
update ui st =
    let
        st_ =
            case ui of
                ZoomIn ->
                    zoomTo (st.md.zoom * 1.2) st

                ZoomReset ->
                    zoomTo 1.0 st

                ZoomOut ->
                    zoomTo (max 0.75 (st.md.zoom / 1.2)) st

                WindowResize window ->
                    { st | ds = makeScreenDimensions window }

                EnablePreview mouse ->
                    preview mouse (enablePreview (stopSplashImmediately st))

                UpdatePreview mouse ->
                    preview mouse st

                Select mouse ->
                    select mouse (disablePreview st)

                Animate delta ->
                    animate delta st

                ToggleControls ->
                    { st | controls = not st.controls }
    in
        ( st_, Cmd.none )


zoomTo : Float -> Model -> Model
zoomTo zoom st =
    { st
        | md = zoomMandelbrotDomain zoom st.md
    }


stopSplashImmediately : Model -> Model
stopSplashImmediately st =
    { st | splashScreenAlpha = Nothing }


disablePreview : Model -> Model
disablePreview st =
    { st | preview = Nothing }


enablePreview : Model -> Model
enablePreview st =
    { st
        | preview =
            Just
                { baseColor = 0.0
                , orbital = emptyOrbital
                }
    }


preview : Mouse.Position -> Model -> Model
preview mouse st =
    let
        createPreview _ =
            let
                start =
                    projectIntoMandelbrot mouse st.ds st.md
            in
                { baseColor = getBaseColor start st.md.r_range
                , orbital = mandelbrotOrbital maxItersPreview start
                }
    in
        { st | preview = Maybe.map createPreview st.preview }


select : Mouse.Position -> Model -> Model
select mouse st =
    let
        selection =
            projectIntoMandelbrot mouse st.ds st.md

        maxIters =
            min maxItersSelection (maxPoints - totalPoints st)

        orbital =
            mandelbrotOrbital maxIters selection
    in
        case orbital.segments of
            [] ->
                st

            _ :: _ ->
                let
                    ttl =
                        (if orbital.inside then
                            liveTimeFactorInside
                         else
                            liveTimeFactorOutside
                        )
                            * orbital.length

                    sat =
                        scale ( 0, toFloat maxItersSelection )
                            ( 0.3, 0.8 )
                            (toFloat (length orbital.segments))

                    animatedTrace =
                        { orbital = orbital
                        , ttl = ttl
                        , age = 0
                        , color = ( getBaseColor selection st.md.r_range, sat, 0.5 )
                        }
                in
                    { st
                        | traces =
                            animatedTrace :: st.traces
                    }


getBaseColor : Complex -> Complex -> Float
getBaseColor z r_range =
    let
        hue =
            scale r_range ( 400, 1500 ) (absC z)
    in
        degrees hue


animate : Time -> Model -> Model
animate dt =
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

        fadeSplashscreen st =
            let
                fade x =
                    x - inSeconds dt * splashScreenFadeSpeed

                removeSplash x =
                    if x < epsilon then
                        Nothing
                    else
                        Just x

                newAlpha =
                    Maybe.map fade st.splashScreenAlpha
                        |> Maybe.andThen removeSplash
            in
                { st | splashScreenAlpha = newAlpha }
    in
        fadeSplashscreen >> updateAge >> removeOld



-- RENDERING


view : Model -> Html.Html Msg
view st =
    Html.div [] <|
        splashScreenView st <|
            [ viewZoomControls st
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
                                    scale ( 0, 1 / st.md.zoom ) minScreenRange 1.0
                            ]
                in
                    layers
                        [ domainCircle
                        , diagrams
                        ]
            ]


splashScreenView : Model -> List (Html.Html a) -> List (Html.Html a)
splashScreenView st v =
    case st.splashScreenAlpha of
        Nothing ->
            v

        Just alpha ->
            let
                f =
                    0.66

                px x =
                    toString (round x) ++ "px"

                l =
                    px (toFloat st.ds.screen_w * (1 - f) * 0.5)

                t =
                    px (toFloat st.ds.screen_h * (1 - f) * 0.5)

                w =
                    px (toFloat st.ds.screen_w * f)

                h =
                    px (toFloat st.ds.screen_h * f)
            in
                (Html.div
                    [ Html.Attributes.style
                        [ ( "position", "absolute" )
                        , ( "z-index", "3" )
                        , ( "width", w )
                        , ( "left", l )
                        , ( "top", t )
                        , ( "height", h )
                        , ( "opacity", toString alpha )
                        ]
                    ]
                    [ Html.div
                        [ Html.Attributes.style
                            [ ( "display", "table-cell" )
                            , ( "vertical-align", "middle" )
                            , ( "text-align", "center" )
                            , ( "text-shadow", "2px 2px #FF0000" )
                            , ( "width", w )
                            , ( "height", h )
                            , ( "line-height", "normal" )
                            , ( "color", "yellow" )
                            , ( "font-family", "\"Times New Roman\", Times, serif" )
                            , ( "font-size", "3em" )
                            ]
                        ]
                        [ Html.text """Move the mouse over the black circle,
                        then press and hold the mouse button, move around and
                        release the button...""" ]
                    ]
                )
                    :: v


viewZoomControls : Model -> Html.Html Msg
viewZoomControls st =
    let
        controlsButton =
            Html.button
                [ Html.Events.onClick ToggleControls ]
                [ Html.text
                    (if st.controls then
                        "Hide Controls"
                     else
                        "Show Controls"
                    )
                ]
    in
        Html.div
            [ Html.Attributes.style
                [ ( "position", "fixed" )
                , ( "color", "gray" )
                , ( "z-index", "2" )
                , ( "padding", "0.5em" )
                , ( "background-color", "white" )
                ]
            ]
            [ if st.controls then
                Html.table [ Html.Attributes.style [ ( "width", "25%" ) ] ]
                    [ Html.tr []
                        [ Html.td [ Html.Attributes.colspan 2 ]
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
                        [ Html.td [ Html.Attributes.colspan 2 ]
                            [ Html.text " Time till next cleanup" ]
                        , Html.td
                            []
                            [ Html.text
                                (toString (round (timeToNextTraceDeath st)))
                            ]
                        ]
                    , Html.tr []
                        [ Html.td []
                            [ Html.button [ Html.Events.onClick ZoomIn ]
                                [ Html.text "+" ]
                            ]
                        , Html.td []
                            [ Html.button [ Html.Events.onClick ZoomReset ]
                                [ Html.text (toString (round (st.md.zoom * 1000))) ]
                            ]
                        , Html.td []
                            [ Html.button
                                [ Html.Events.onClick ZoomOut ]
                                [ Html.text "-" ]
                            ]
                        ]
                    , Html.tr []
                        [ Html.td
                            [ Html.Attributes.colspan 3 ]
                            [ controlsButton ]
                        ]
                    ]
              else
                controlsButton
            ]


renderPreview : Model -> List Form
renderPreview st =
    case st.preview of
        Nothing ->
            []

        Just p ->
            let
                lineStyle =
                    if p.orbital.inside then
                        dotted (Color.hsla p.baseColor 0.7 0.7 0.5)
                    else
                        dashed (Color.hsla p.baseColor 0.5 0.7 0.5)
            in
                [ traced lineStyle <|
                    path <|
                        List.map (\s -> projectToScreen s st.md st.ds) <|
                            segmentsToPath <|
                                reverse p.orbital.segments
                ]


renderSelection : Model -> List Form
renderSelection st =
    concatMap (renderAnimatedOrbital st) st.traces


renderAnimatedOrbital : Model -> AnimatedOrbital -> List Form
renderAnimatedOrbital st t =
    concatMap
        (renderAnimatedOrbitalSegment
            st
            t.age
            t.orbital.inside
            t.color
        )
        t.orbital.segments


renderAnimatedOrbitalSegment :
    Model
    -> Float
    -> Bool
    -> ( Float, Float, Float )
    -> OrbitalSegment
    -> List Form
renderAnimatedOrbitalSegment st age insideMandelbrot ( hue, sat, lum ) l =
    let
        glow =
            (age * velocity - l.distance) / l.length

        glowing =
            glow > 0 && glow < 1

        warmingUp =
            glow <= 0

        coolingDown =
            glow >= 1

        screenFrom =
            projectToScreen l.from st.md st.ds

        screenTo =
            projectToScreen l.to st.md st.ds

        screenLength =
            distC screenTo screenFrom

        heat =
            if glowing then
                1
            else if coolingDown then
                1 / logBase 2 glow
            else if warmingUp then
                -1 / (glow - 1)
            else
                0

        lineColor =
            let
                intensity =
                    if warmingUp then
                        0.8 * heat + 0.2
                    else if coolingDown && glow <= 2 then
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
                        screenLength
                            / 4
                            * if glowing then
                                glow
                              else
                                1

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
                    ++ drawMarkerCircleShadow
        else if coolingDown then
            [ drawLine lineColor screenFrom screenTo
            ]
                ++ drawMarkerCircleShadow
        else
            []


totalPoints : Model -> Int
totalPoints st =
    List.foldr
        (\t s -> s + List.length t.orbital.segments)
        0
        st.traces


timeToNextTraceDeath : Model -> Float
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Mouse.ups Select
        , Mouse.moves UpdatePreview
        , Mouse.downs EnablePreview
        , Window.resizes WindowResize
        , AnimationFrame.diffs Animate
        ]



-- DEFAULTS


defaultZoom : Float
defaultZoom =
    1.375


liveTimeFactorInside : Float
liveTimeFactorInside =
    2 / velocity


liveTimeFactorOutside : Float
liveTimeFactorOutside =
    0.3 / velocity


maxItersPreview : Int
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


splashScreenFadeSpeed : Float
splashScreenFadeSpeed =
    0.2


epsilon : Float
epsilon =
    0.0001



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
