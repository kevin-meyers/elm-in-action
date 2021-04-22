module PhotoGroove exposing (main)

import Browser
import Html exposing (Attribute, Html, node, button, div, h1, h3, img, input, label, text)
import Html.Attributes as Attr exposing (src, property, title, classList, type_, name, checked, class, id)
import Html.Events exposing (on, onClick)

import Random
import Html exposing (select)

import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }

type Status = Loading | Loaded (List Photo) String | Errored String

type Msg
    = ClickedPhoto String 
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe 
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


type ThumbnailSize
    = Small
    | Medium
    | Large

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider = node "range-slider"

-- haskell syntax version, I like elms quite a bit
-- onSlide :: (Int -> msg) -> Attribute msg
-- onSlide toMsg = on "slide" decoderMsg
--   where
--     decoderMsg = toMsg <$> at ["detail", "userSlidTo"] int

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div 
        [ class "filter-slider" ] [ label [] [ text name ]
        , rangeSlider 
            [ Attr.max "11"
            , property "val" <| Encode.int magnitude
            , onSlide toMsg
            ] []
        , label [] [ text <| String.fromInt magnitude]
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url )
        ], onClick (ClickedPhoto thumb.url) ] []


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size), checked (size == chosenSize)] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
           
            Loading ->
                []
                
            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]

    
viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter "Hue" 0
        , viewFilter "Ripple" 0
        , viewFilter "Noise" 0
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl)] []
    ]


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        
        Loading ->
            status
        
        Errored _ ->
            status
        

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto
                    |> Tuple.pair model

                Loaded [] _ ->
                    (model, Cmd.none)
                
                Loading ->
                    (model, Cmd.none)
                
                Errored errorMessage ->
                    (model, Cmd.none)
                
        ClickedSize size ->
            ({ model | chosenSize = size }, Cmd.none)
        
        GotRandomPhoto photo ->
            ({ model | status = selectUrl photo.url model.status }, Cmd.none)

        GotPhotos (Ok photos) ->
            case photos of
                first :: _ ->
                    ( { model | status = Loaded photos first.url }, Cmd.none)
                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none)

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = urlPrefix ++ "photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }

initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }

main : Program () Model Msg
main =
    Browser.element { init = \_ -> (initialModel, initialCmd), view = view, update = update, subscriptions = \_ -> Sub.none }