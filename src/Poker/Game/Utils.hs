{-# LANGUAGE CPP #-}

module Poker.Game.Utils where

import Control.Arrow ((>>>))
import Control.Lens
  ( At (at),
    Each (each),
    Identity (runIdentity),
    Ixed (ix),
    Traversal',
    lens,
    makeLenses,
    mapped,
    non,
    preuse,
    to,
    uncons,
    use,
    view,
    (%=),
    (&),
    (+=),
    (-=),
    (.=),
    (<%=),
    (<&>),
    (<<.=),
    (?=),
    _Just,
  )
import Control.Monad.Except
  ( Except,
    ExceptT,
    MonadError,
    runExcept,
    runExceptT,
    throwError,
  )
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import qualified Data.Text as T
import Debug.Trace
import Poker
import Poker.Game.AvailableActions
  ( actionMatches,
    availableActions,
  )
import Poker.Game.Types

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Render.String
#else
import           Data.Text.Prettyprint.Doc ( Pretty(pretty)
                                                , defaultLayoutOptions
                                                , layoutPretty
                                                )
import           Data.Text.Prettyprint.Doc.Render.String
#endif

type IsGame m b =
  ( IsBet b,
    Pretty b,
    Show b,
    Ord b,
    MonadState (GameState b) m,
    MonadError (GameError b) m
  )

incPot :: (IsBet b, MonadState (GameState b) m) => b -> m ()
incPot bet = potSize . mapped %= add bet

getPlayer :: MonadReader (GameState b) m => Position -> m (Maybe (Player b))
getPlayer pos_ = ask <&> view (posToPlayer . at pos_)

-- Increase stack size at a seat position
incStack :: (IsGame m b) => Position -> b -> Action b -> m ()
incStack pos amount a = do
  _ <- getStack a pos
  -- mErrorAssert (playerStack - amount > 0) (NegativePlayerStack badAct)
  atPlayerStack pos %= add amount

-- Decrease stack size at a seat position
decStack ::
  (IsGame m b, MonadState (GameState t) m) =>
  Position ->
  b ->
  Action b ->
  m ()
decStack pos amount badAct = do
  playerStack <- getStack badAct pos
  newPlayerStack <-
    maybeToError
      (NegativePlayerStack badAct)
      (playerStack `minus` amount)
  atPlayerStack pos .= newPlayerStack

atPlayerStack :: Position -> Traversal' (GameState t) t
atPlayerStack pos =
  posToPlayer . ix pos . stack . lens _unStack (\_ s -> Stack s)

getStack :: IsGame m b => Action b -> Position -> m b
getStack a pos =
  maybeToError PlayerNotFound =<< preuse (atPlayerStack pos)

maybeToError :: IsGame m b => GameError b -> Maybe a -> m a
maybeToError e mb = case mb of
  Just a -> return a
  Nothing -> throwError e
