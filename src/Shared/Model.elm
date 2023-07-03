module Shared.Model exposing (Model)

import Game exposing (Game)


type alias Model =
    { existingGame : Maybe Game
    }
