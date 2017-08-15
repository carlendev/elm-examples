import Html exposing (Html, Attribute, button, div, text, br, input, select, option, p, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

type alias Model =
    { number : Int
    , content : String
    , incrementor : Int
    , error : List String                
    }

model : Model
model =
    { number = 0
    , content = ""
    , incrementor = 1
    , error = []                
    }

type Msg =
    Incr
    | Decr
    | Reset
    | Incrementor String
    | Change String

increment : Model -> Int
increment {number, incrementor} =
    number + incrementor

decrement : Model -> Int
decrement {number, incrementor} =
    number - incrementor

update : Msg -> Model -> Model
update msg model =
    case msg of
        Incr ->
            { model | number = increment model }
        Decr ->
            if .number model == 0 then
                { model | number = 0 }
            else        
                { model | number = decrement model }
        Reset ->
            { model | number = 0 }
        Incrementor val ->
            case String.toInt val of
                Err msg -> { model | error = [ msg ] }
                Ok val -> { model | incrementor = val }
        Change str ->
            { model | content = str }
               

inputExample : Model -> Html Msg
inputExample model =                
         div []
             [ button [ onClick Decr] [ text "-"]
             , div [] [ text (toString (.number model)) ]      
             , button [ onClick Incr] [ text "+"]
             , br [] []
             , button [ onClick Reset ] [ text "Reset" ]
             , p [] [ text "Select increment value" ]
             , ul [] (List.map toLi (.error model))   
             , select [ placeholder "0", onInput Incrementor ] (List.map selectOption (List.range 1 10)) ]
             
view : Model -> Html Msg
view model =
    div []
        [
         inputExample model
        , div []
            [ input [ placeholder "Text to reverse", onInput Change ] []
            , div [] [ text (String.reverse (.content model)) ]
            ]
        ]

selectOption: Int -> Html Msg
selectOption val =
    let
        stringVal = toString val
    in                
        option [ value stringVal ] [ text stringVal ]

toLi: String -> Html Msg
toLi val =
    li [] [ text val ]
        
main =
    Html.beginnerProgram { model = model, view = view, update = update }
