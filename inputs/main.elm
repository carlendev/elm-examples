import Html exposing (Html, button, div, text, br)
import Html.Events exposing (onClick)

type alias Model =
    {
        number : Int
        , content : String
    }


model : Model
model =
    {
        number = 0
        , content = ""
    }

type Msg = Incr | Decr | Reset

increment : Model -> Int
increment {number} =
    number + 1

decrement : Model -> Int
decrement {number} =
    number - 1 

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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decr] [ text "-"]
        , div [] [ text (toString (.number model)) ]      
        , button [ onClick Incr] [ text "+"]
        , br [] []
        , button [ onClick Reset ] [ text "Reset" ] ]
        

main =
    Html.beginnerProgram { model = model, view = view,  update = update }
