{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.Game.Bovada
   where

import Control.Lens
    ( (<&>),
      uncons,
      preuse,
      use,
      view,
      non,
      (<%=),
      (<<.=),
      _Just,
      (%=),
      (+=),
      (-=),
      (.=),
      (?=),
      mapped,
      makeLenses,
      Identity(runIdentity),
      At(at),
      Ixed(ix),
      Each(each),
      Traversal' )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Poker.Base
import           Poker.Game.Types

type IsGame m
  = (MonadState (GameState BigBlind) m, MonadError (GameErrorBundle BigBlind) m)


incPot :: (MonadState (GameState BigBlind) m) => BigBlind -> m ()
incPot bet = potSize . mapped += bet

getPlayer
  :: MonadReader (GameState BigBlind) m
  => Position
  -> m (Maybe (Player BigBlind))
getPlayer pos_ = ask <&> view (posToPlayer . at pos_)

-- Reduce state pot size
decPot :: (IsGame m) => BigBlind -> m (PotSize BigBlind)
decPot amount = do
  checkPotSize <- potSize <%= dec amount
  mErrorAssert (checkPotSize >= PotSize 0) NegativePotSize
  return checkPotSize
 where
  dec :: BigBlind -> PotSize BigBlind -> PotSize BigBlind
  dec betSize (PotSize potSize_) = PotSize $ potSize_ - betSize

-- Increase stack size at a seat position
incStack :: (IsGame m) => Position -> BigBlind -> Action BigBlind -> m ()
incStack pos amount _ = do
  _ <- getStack pos
  -- mErrorAssert (playerStack - amount > 0) (NegativePlayerStack badAct)
  atPlayerStack pos -= amount

-- Decrease stack size at a seat position
decStack
  :: (IsGame m, MonadState (GameState t) m)
  => Position
  -> BigBlind
  -> Action BigBlind
  -> m ()
decStack pos amount badAct = do
  playerStack <- getStack pos
  mErrorAssert (playerStack - amount >= 0) (NegativePlayerStack badAct)
  atPlayerStack pos -= amount

atPlayerStack :: Position -> Traversal' (GameState t) t
atPlayerStack pos = posToPlayer . ix pos . stack . unStack

getStack :: IsGame m => Position -> m BigBlind
getStack pos = maybeToErrorBundle PlayerNotFound =<< preuse (atPlayerStack pos)

-- Not a big deal but this implementation of the player queue
-- means that every rotation takes 6 steps since snoc is O(n)
-- Ensures that the position removed from queue is the one in the queue currently
rotateNextActor :: IsGame m => Position -> m ()
rotateNextActor pos = do
  (toAct, rest) <-
    maybeToErrorBundle NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest ++ [toAct]

-- Ensures that the next actor is the one acting
-- Removes the next actor instead of put it at the back of the queue
removeNextActor :: IsGame m => Position -> m ()
removeNextActor pos = do
  (toAct, rest) <-
    maybeToErrorBundle NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest

numActivePlayers :: IsGame m => m Int
numActivePlayers = length <$> use toActQueue

{- IsGame Actions -}

-- Monadic action to alter state board with a deal action
addDealToState :: IsGame m => DealerAction -> m ()
addDealToState deal = do
  board <- use street
  case addDealToBoard deal board of
    Nothing     -> throwBundleError $ IncorrectDeal deal board
    Just board' -> street .= board'
 where
    -- Take a deal data type and return a board
    -- Returns Nothing if the Deal is illegal
  addDealToBoard :: DealerAction -> Board -> Maybe Board
  addDealToBoard PlayerDeal board@InitialTable = Just $ PreFlopBoard board
  addDealToBoard (FlopDeal c1 c2 c3) board@(PreFlopBoard _) =
    Just $ FlopBoard (c1, c2, c3) board
  addDealToBoard (TurnDeal card) board@(FlopBoard _ _) =
    Just $ TurnBoard card board
  addDealToBoard (RiverDeal card) board@(TurnBoard _ _) =
    Just $ RiverBoard card board
  addDealToBoard _ _ = Nothing

-- Monadic action to execute an Action datatype on the given state
emulateAction
  :: forall m
   . (IsGame m, MonadState (GameState BigBlind) m)
  => Action BigBlind
  -> m ()
emulateAction a = case a of
  MkPlayerAction (PlayerAction pos actVal _) -> do
    use street >>= \case
      InitialTable -> throwBundleError (PlayerActedPreDeal a)
      _            -> pure ()
    betSize <- flip (getBetSize pos) actVal
      =<< use (streetInvestments . at pos . non 0)
    incPot betSize
    when (isAggressive actVal) $ aggressor ?= pos
    processActiveBet actVal
    processInvestment pos betSize
    decStack pos betSize a
    doRotateNextActor pos actVal
  MkDealerAction deal -> do
    addDealToState deal
    saveAggressor
    case deal of
      PlayerDeal -> pure ()
      _          -> activeBet .= Nothing
    use street >>= \case
      InitialTable   -> toActQueue %= sortPreflop
      PreFlopBoard _ -> toActQueue %= sortPreflop
      _              -> do
        streetInvestments . each .= 0
        toActQueue %= sortPostflop
  MkTableAction act -> handleTableAction act
 where
  saveAggressor :: IsGame m => m ()
  saveAggressor = (lastStreetAggressor .=) =<< aggressor <<.= Nothing
  isAggressive :: BetAction t -> Bool
  isAggressive = \case
    Call _         -> False
    Raise      _ _ -> True
    AllInRaise _ _ -> True
    Bet   _        -> True
    AllIn _        -> False
    Fold           -> False
    Check          -> False
    OtherAction    -> False
  getBetSize :: Position -> BigBlind -> BetAction BigBlind -> m BigBlind
  getBetSize pos previousInvestment act = do
    activeBet <- fromMaybe 0 <$> preuse (activeBet . _Just . amountFaced)
    case act of
      Call amount -> do
        mErrorAssert (amount == activeBet - previousInvestment)
          $ CallWrongAmount activeBet a
        pure amount -- - previousInvestment
      Raise      _ amountTo -> pure $ amountTo - previousInvestment
      AllInRaise _ amountTo -> pure $ amountTo - previousInvestment
      Bet   amount          -> pure amount
      AllIn amount          -> do
        playerStack <- getStack pos
        mErrorAssert (amount == playerStack) (AllInNotFullStack playerStack a)
        pure amount
      Fold        -> pure 0
      Check       -> pure 0
      OtherAction -> pure 0
  processInvestment :: (IsGame m) => Position -> BigBlind -> m ()
  processInvestment pos betSize =
    streetInvestments . at pos . non 0 %= (+ betSize)
  doRotateNextActor :: IsGame m => Position -> BetAction t -> m ()
  doRotateNextActor pos =
    let doRemove = removeNextActor pos
        doRotate = rotateNextActor pos
    in  \case
          Call _         -> doRotate
          Raise      _ _ -> doRotate
          AllInRaise _ _ -> doRemove
          Bet   _        -> doRotate
          AllIn _        -> doRemove
          Fold           -> doRemove
          Check          -> doRotate
          OtherAction    -> pure () -- TODO
  processActiveBet :: (IsGame m) => BetAction BigBlind -> m ()
  processActiveBet = \case
    Call _                -> pure ()
    Raise      _ amountTo -> incActiveBet amountTo
    AllInRaise _ amountTo -> incActiveBet amountTo
    Bet   amount          -> incActiveBet amount
    AllIn amount          -> do
      faced <- fromMaybe 0 <$> preuse (activeBet . _Just . amountFaced)
      if amount > faced then incActiveBet amount else pure ()
    Fold        -> pure ()
    Check       -> pure ()
    OtherAction -> pure ()
  incActiveBet :: IsGame m => BigBlind -> m ()
  incActiveBet newFacedAmount = use activeBet >>= \case
    Nothing -> activeBet ?= ActionFaced OneB newFacedAmount undefined
    Just (ActionFaced bType _ _) ->
      activeBet ?= ActionFaced (succ bType) newFacedAmount undefined
  handleTableAction :: (IsGame m) => TableAction BigBlind -> m ()
  handleTableAction UnknownAction         = pure () -- FIXME
  handleTableAction (TableAction pos val) = case val of
    Deposit  _        -> return () -- TODO may need to inc stack size
    Post     postSize -> doPost postSize
    PostDead postSize -> doPost postSize
    Enter             -> return () -- Not sure what this should be
    Leave ->
      -- seat <- maybeToErrorBundle (SeatNotFound pos)
      --         =<< use (posToPlayer . at pos)
      -- posToPlayer %= M.delete pos
      -- playerMap %= M.delete seat
      -- toActQueue %= List.delete pos
      return ()
    SitOut        -> return ()
    SitDown       -> return ()
    Showdown _ _  -> return ()
    Muck     _ _  -> return ()
    Rejoin        -> return ()
    Return amount -> do
      _ <- decPot amount
      incStack pos amount a
    Result _ -> return () -- TODO
   where
    doPost :: (IsGame m) => BigBlind -> m ()
    doPost postSize = do
      incPot postSize
      streetInvestments . at pos ?= postSize
      preuse (activeBet . _Just . amountFaced) >>= \case
        Just amountFaced' | postSize >= amountFaced' ->
          activeBet ?= ActionFaced PostB postSize undefined
        Just _  -> pure ()
        Nothing -> activeBet ?= ActionFaced PostB postSize undefined
      decStack pos postSize a

mErrorAssert :: IsGame m => Bool -> GameError BigBlind -> m ()
mErrorAssert b e = if b then return () else throwBundleError e

throwBundleError :: IsGame m => GameError BigBlind -> m a
throwBundleError e = throwError . GameErrorBundle e =<< get

maybeToErrorBundle :: IsGame m => GameError BigBlind -> Maybe a -> m a
maybeToErrorBundle e mb = case mb of
  Just a  -> return a
  Nothing -> throwError . GameErrorBundle e =<< get

{- Testing and Printing methods -}

execIsGame :: StateT s (ExceptT e Identity) a -> s -> Either e s
execIsGame m = runIdentity . runExceptT . execStateT m

evalIsGame :: StateT s (ExceptT e Identity) a -> s -> Either e a
evalIsGame m = runIdentity . runExceptT . evalStateT m

-- testIsGame
--   :: MonadIO m  =>
--      StateT GameState (ExceptT e m) a -> GameState -> m (Either e String)
testIsGame
  :: Show BigBlind
  => StateT (GameState BigBlind) (ExceptT (GameErrorBundle BigBlind) IO) a
  -> GameState BigBlind
  -> IO ()
testIsGame actionM inputState = do
  res <- runExceptT (runStateT actionM inputState)
  case res of
    Left e -> do
      putStrLn "Game Failed"
      -- prettyPrint e
    Right (_, state') -> do
      putStrLn "*** Game completed without error ***"
      -- prettyPrint state'
      putStr $ view stateHandText state'

