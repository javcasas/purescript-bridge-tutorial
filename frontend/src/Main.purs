module Main where

import App.Routes (match)
import App.Scientist as Scientist
import App.Layout (Action(PageView, ScientistsLoaded, Nop), State, view, update)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude (bind, pure, ($), unit, (<>))
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)
import Signal.Channel (channel, Channel, subscribe, CHANNEL, send)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Either (either)


type AppEffects = (dom :: DOM, console :: CONSOLE, ajax :: AJAX)

-- | App configuration
config :: forall eff. State -> Eff (ajax :: AJAX, dom :: DOM, channel :: CHANNEL, console :: CONSOLE | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  chan <- channel Nop
  let signal = subscribe chan
  let dataArrivedSignal = signal ~> \a -> a
  runAff (\e -> pure unit) (\s -> send chan s) do
    res <- affjax $ defaultRequest { url = "http://localhost:8080/scientist", method = Left GET }
    pure $ ScientistsLoaded $ Scientist.State {
      scientists: either (\_ -> []) (\a -> a) $ decodeJson res.response
      , current: 0
    }


  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [routeSignal, dataArrivedSignal] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app
