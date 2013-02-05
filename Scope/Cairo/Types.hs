{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall #-}

module Scope.Cairo.Types (
    -- * Types
      ViewCairo(..)
    , ScopeDiag
) where

import qualified Graphics.UI.Gtk as G
import Scope.Layer
import Scope.Types

import Control.Applicative
import qualified Diagrams.Backend.Cairo  as B
import qualified Diagrams.Backend.Gtk    as B
import Diagrams.Prelude (Monoid', Diagram, R2)

----------------------------------------------------------------------

data ViewCairo = ViewCairo
    { frame  :: G.VBox
    , canvas :: G.DrawingArea
    , adj    :: G.Adjustment
    }

type ScopeDiag ui = Scope (Diagram B.Cairo R2) ui
