import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Model =
  { count : Int
  , message : String
  , member : Member
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member 0 "Mathia" "mathias@mail.dk")
  , Cmd.none
  )
-- datatype with keys and values, alias is a kind of a shortcut so
-- we don't have to repeat the code
-- member is located in the model and I can access it through it 
type alias Member = 
  {
  id: Int,
  name: String,
  email: String
  }

-- UPDATE

type Msg -- This is a union type, it's either one of these choices, cannot be 2 at the same time
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember
  | MemberReceived (Result Http.Error Member)
  | Id String
  | Name String
  | Email String
  | PostMember
  | MemberPosted (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember ->
      (model, getMember)

    MemberReceived (Ok newMember) ->
    ({ model | member = newMember}, Cmd.none)

    MemberReceived (Err error) ->
      ( {model | message = toString error }, Cmd.none)

    Id id ->
      case String.toInt id of 
        Ok num -> 
          ({model | member = (Member num model.member.name model.member.email)}, Cmd.none)
        Err error ->
          ({model | member = (Member 0 model.member.name model.member.email),
                    message = "NO WORKING, FIX PLS"}, Cmd.none)

    Name name ->
      ({model | member = (Member model.member.id name model.member.email)}, Cmd.none)

    Email email ->
      ({model | member = (Member model.member.id model.member.name email)}, Cmd.none)
    
    PostMember-> 
      (model, postMember model.member)
    
    MemberPosted (Ok msg) ->
      ({model | message = "OK"}, getMemberCount)

    MemberPosted (Err error) ->
      ({model | message = toString error}, Cmd.none)

    
    


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick GetMember ] [ text "Get a member" ]
    , button [onClick PostMember] [text "Post a member"]
    , hr [] []
    , text model.message
    , hr [] []
    , input [ type_ "text", placeholder "ID", onInput Id, value (toString model.member.id)] []
    , input [ type_ "text", placeholder "Name", onInput Name, value model.member.name] []
    , input [ type_ "text", placeholder "Email", onInput Email, value model.member.email] []
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember : Cmd Msg
getMember = 
    Http.send MemberReceived (Http.get (url "1") decodeMember)

memberJsonBody: Member -> Http.Body
memberJsonBody member = 
  Http.jsonBody <| encodeMember member

postMember: Member -> Cmd Msg
postMember member = 
  Http.send MemberPosted (Http.post (url "") (memberJsonBody member) Decode.string)

--DECODER

decodeMember: Decode.Decoder Member
decodeMember = 
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)

--ENCODER

encodeMember: Member -> Encode.Value
encodeMember member = 
    Encode.object
    [("id", Encode.int member.id),
    ("name", Encode.string member.name),
    ("email", Encode.string member.email)]
