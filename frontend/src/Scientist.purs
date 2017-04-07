module App.Scientist where

import Prelude ((+), (-), const, show, ($), (<=), (>=), bind, pure, (<>), (<$>))
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode ((.?))
import Data.Argonaut.Core (toObject)
import Data.Either (Either(..))

data Action = Next | Previous

data Scientist = Scientist {
                 id :: Int
               , firstName :: String
               , lastName :: String
               }

instance decodeScientist :: DecodeJson Scientist where
  decodeJson j = case toObject j of
                  Just o -> do
                    id <- o .? "sId"
                    firstName <- o .? "sFirstName"
                    lastName <- o .? "sLastName"
                    pure $ Scientist {
                        id: id,
                        firstName: firstName,
                        lastName: lastName
                      }
                  Nothing -> Left "Noparse"

data State = State {
  scientists :: Array Scientist
, current :: Int
}

init :: State
init = State {
  scientists : []
, current : 0
}

update :: Action -> State -> State
update Previous (State s) = State $ s {current = bounded_current}
  where
    ncurrent = s.current - 1
    bounded_current = if ncurrent <= 0
                      then 0
                      else ncurrent
update Next (State s) = State $ s {current=bounded_current}
  where
    ncurrent = s.current + 1
    max_current = length s.scientists - 1
    bounded_current = if ncurrent >= max_current
                      then max_current
                      else ncurrent

view :: State -> Html Action
view (State {scientists:[]}) =
  div []
    [
      span [] [text "Loading scientists..."]
    ]
view (State s) =
  let mScientist = s.scientists !! s.current in
  case mScientist of
    Nothing -> view (State {scientists: [], current: 0})
    Just (Scientist scientist) ->
      div []
        [
          div [] [
            span [] [ text $ (show s.current) <> " " <> scientist.firstName <> " " <> scientist.lastName ]
          ]
        , button [ onClick (const Previous) ] [ text "Prev. Scientist" ]
        , button [ onClick (const Next) ] [ text "Next Scientist" ]
        ]
