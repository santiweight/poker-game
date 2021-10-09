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

getPlayer :: MonadReader (GameState b) m => Position -> m (Maybe (Stack b))
getPlayer pos_ = ask <&> view (posToStack . at pos_)

incStack :: (IsGame m b) => Position -> b -> Action b -> m ()
incStack pos amount a = do
  _ <- getStack a pos
  atPlayerStack pos %= add amount

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
  posToStack . ix pos . lens _unStack (\_ s -> Stack s)

getStack :: IsGame m b => Action b -> Position -> m b
getStack a pos =
  maybeToError PlayerNotFound =<< preuse (atPlayerStack pos)

-- Not a big deal but this implementation of the player queue
-- means that every rotation takes 6 steps since snoc is O(n)
-- Ensures that the position removed from queue is the one in the queue currently
rotateNextActor :: IsGame m b => Action b -> Position -> m ()
rotateNextActor a pos = do
  (toAct, rest) <-
    maybeToError NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest ++ [toAct]

-- Ensures that the next actor is the one acting
-- Removes the next actor instead of put it at the back of the queue
removeNextActor :: IsGame m b => Action b -> Position -> m ()
removeNextActor a pos = do
  (toAct, rest) <-
    maybeToError NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest

numActivePlayers :: IsGame m b => m Int
numActivePlayers = length <$> use toActQueue

mErrorAssert :: IsGame m b => Bool -> GameError b -> m ()
mErrorAssert b e = if b then return () else throwError e

maybeToError :: IsGame m b => GameError b -> Maybe a -> m a
maybeToError e mb = case mb of
  Just a -> return a
  Nothing -> throwError e
