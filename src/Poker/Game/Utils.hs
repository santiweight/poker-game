{-# LANGUAGE CPP #-}

module Poker.Game.Utils where

import Control.Lens
import Control.Monad.Except
  ( MonadError,
    throwError, Except, runExcept
  )
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Poker
import Poker.Game.Types

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
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

incStack :: (IsGame m b) => Position -> b -> m ()
incStack pos amount = do
  _ <- getStack pos
  atPlayerStack pos %= add amount

decStack ::
  (IsGame m b, MonadState (GameState b) m) =>
  Position ->
  b ->
  m ()
decStack pos amount = do
  playerStack <- getStack pos
  newPlayerStack <-
    maybeToError
      NegativePlayerStack
      (playerStack `minus` amount)
  atPlayerStack pos .= newPlayerStack

atPlayerStack :: Position -> Traversal' (GameState t) t
atPlayerStack pos =
  posToStack . ix pos . lens _unStack (\_ s -> Stack s)

getStack :: IsGame m b => Position -> m b
getStack pos =
  maybeToError PlayerNotFound =<< preuse (atPlayerStack pos)

-- Not a big deal but this implementation of the player queue
-- means that every rotation takes 6 steps since snoc is O(n)
-- Ensures that the position removed from queue is the one in the queue currently
rotateNextActor :: IsGame m b => Position -> m ()
rotateNextActor pos = do
  (toAct, rest) <-
    maybeToError NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest ++ [toAct]

-- Ensures that the next actor is the one acting
-- Removes the next actor instead of put it at the back of the queue
removeNextActor :: IsGame m b => Position -> m ()
removeNextActor pos = do
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

runGame :: StateT s (Except e) a -> s -> Either e s
runGame m = runExcept . execStateT m
