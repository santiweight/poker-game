{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Poker.Game.Emulate where

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
    MonadError (GameErrorBundle b) m
  )

incPot :: (IsBet b, MonadState (GameState b) m) => b -> m ()
incPot bet = potSize . mapped %= add bet

getPlayer :: MonadReader (GameState b) m => Position -> m (Maybe (Player b))
getPlayer pos_ = ask <&> view (posToPlayer . at pos_)

-- Reduce state pot size
decPot :: (IsGame m b) => Action b -> b -> m (Pot b)
decPot a amount = do
  newPotSizeMay <- use potSize <&> dec amount
  maybeToErrorBundle a NegativePotSize newPotSizeMay
  where
    dec :: IsBet b => b -> Pot b -> Maybe (Pot b)
    dec betSize (Pot potSize_) = Pot <$> potSize_ `minus` betSize

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
    maybeToErrorBundle
      badAct
      (NegativePlayerStack badAct)
      (playerStack `minus` amount)
  atPlayerStack pos .= newPlayerStack

atPlayerStack :: Position -> Traversal' (GameState t) t
atPlayerStack pos =
  posToPlayer . ix pos . stack . lens _unStack (\_ s -> Stack s)

getStack :: IsGame m b => Action b -> Position -> m b
getStack a pos =
  maybeToErrorBundle a PlayerNotFound =<< preuse (atPlayerStack pos)

-- Not a big deal but this implementation of the player queue
-- means that every rotation takes 6 steps since snoc is O(n)
-- Ensures that the position removed from queue is the one in the queue currently
rotateNextActor :: IsGame m b => Action b -> Position -> m ()
rotateNextActor a pos = do
  (toAct, rest) <-
    maybeToErrorBundle a NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert a (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest ++ [toAct]

-- Ensures that the next actor is the one acting
-- Removes the next actor instead of put it at the back of the queue
removeNextActor :: IsGame m b => Action b -> Position -> m ()
removeNextActor a pos = do
  (toAct, rest) <-
    maybeToErrorBundle a NoPlayersInQueue . uncons =<< use toActQueue
  mErrorAssert a (pos == toAct) $ WrongPlayerActed toAct pos
  toActQueue .= rest

numActivePlayers :: IsGame m b => m Int
numActivePlayers = length <$> use toActQueue

{- IsGame Actions -}

-- Monadic action to alter state board with a deal action
addDealToState :: IsGame m b => Action b -> DealerAction -> m ()
addDealToState a deal = do
  board <- use street
  case addDealToBoard deal board of
    Nothing -> throwBundleError a $ IncorrectDeal deal board
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
emulateAction :: forall m b. (IsGame m b) => Action b -> m ()
emulateAction a = do
  res <- case a of
    MkPlayerAction pa@(PlayerAction pos actVal) -> do
      availActions <- get <&> availableActions

      use street >>= \case
        InitialTable -> throwBundleError a (PlayerActedPreDeal a)
        _ -> pure ()
      betSize <-
        flip (getBetSize pos) actVal
          =<< use (streetInvestments . at pos . non mempty)
      incPot betSize
      when (isAggressive actVal) $ aggressor ?= pos
      processActiveBet actVal
      processInvestment pos betSize
      decStack pos betSize a
      -- TODO all non-available actions should maybe have their own errors?
      -- It would be good to silence error on each side (available/emulate)
      -- and then ensure that the same errors are covered by each.
      availActions & \case
        Left txt -> throwBundleError a . CustomError . T.unpack $ txt
        Right (pos, availableActions) ->
          unless (any (actionMatches actVal) availableActions) $
            throwBundleError a
              . CustomError
              $ "Action "
                <> show (prettyString <$> a)
                <> "is not available"

      doRotateNextActor pos actVal
    MkDealerAction deal -> do
      addDealToState a deal
      saveAggressor
      case deal of
        PlayerDeal -> pure ()
        _ -> activeBet .= Nothing
      use street >>= \case
        InitialTable -> toActQueue %= sortPreflop
        PreFlopBoard _ -> toActQueue %= sortPreflop
        _ -> do
          streetInvestments . each .= mempty
          toActQueue %= sortPostflop
    MkPostAction act -> handlePostAction act
  -- when (any actionMatches )
  pure ()
  where
    saveAggressor :: IsGame m b => m ()
    saveAggressor = aggressor .= Nothing
    -- saveAggressor :: IsGame m => m ()
    -- saveAggressor = (lastStreetAggressor .=) =<< aggressor <<.= Nothing
    isAggressive :: BetAction t -> Bool
    isAggressive = \case
      Call _ -> False
      Raise _ _ -> True
      AllInRaise _ _ -> True
      Bet _ -> True
      AllIn _ -> False
      Fold -> False
      Check -> False
    getBetSize :: Position -> b -> BetAction b -> m b
    getBetSize pos previousInvestment act = do
      activeBetSize <-
        fromMaybe mempty
          <$> preuse (activeBet . _Just . amountFaced)
      case act of
        Call amount -> do
          mErrorAssert a (amount `add` previousInvestment == activeBetSize) $
            CallWrongAmount activeBetSize a
          pure amount -- - previousInvestment
        Raise amountBy _ -> pure amountBy
        AllInRaise amountBy _ -> pure amountBy
        Bet amount -> pure amount
        AllIn amount -> do
          playerStack <- getStack a pos
          mErrorAssert a (amount == playerStack) (AllInNotFullStack playerStack a)
          pure amount
        Fold -> pure mempty
        Check -> pure mempty
    processInvestment :: (IsGame m b) => Position -> b -> m ()
    processInvestment pos betSize =
      streetInvestments . at pos . non mempty %= add betSize
    doRotateNextActor :: IsGame m b => Position -> BetAction t -> m ()
    doRotateNextActor pos =
      let doRemove = removeNextActor a pos
          doRotate = rotateNextActor a pos
       in \case
            Call _ -> doRotate
            Raise _ _ -> doRotate
            AllInRaise _ _ -> doRemove
            Bet _ -> doRotate
            AllIn _ -> doRemove
            Fold -> doRemove
            Check -> doRotate
    processActiveBet :: (IsGame m b) => BetAction b -> m ()
    processActiveBet = \case
      Call _ -> pure ()
      Raise amountBy amountTo -> setActiveBet amountBy amountTo
      AllInRaise amountBy amountTo -> setActiveBet amountBy amountTo
      Bet amount -> setActiveBet amount amount
      AllIn amount -> do
        faced <- fromMaybe mempty <$> preuse (activeBet . _Just . amountFaced)
        -- TODO track whether action is opened up. Action is only open if minimum raise
        -- has been reached
        if amount > faced
          then setActiveBet (fromJust $ amount `minus` faced) amount
          else pure ()
      Fold -> pure ()
      Check -> pure ()
    setActiveBet :: IsGame m b => b -> b -> m ()
    setActiveBet raiseBy newFacedAmount =
      use activeBet >>= \case
        Nothing -> activeBet ?= ActionFaced OneB newFacedAmount newFacedAmount
        Just (ActionFaced bType amountFaced _) -> do
          raiseAmount <-
            maybeToErrorBundle
              a
              NewActionFacedLessThanPrevious
              (newFacedAmount `minus` amountFaced)
          activeBet ?= ActionFaced (succ bType) newFacedAmount raiseAmount
    handlePostAction :: (IsGame m b) => PostAction b -> m ()
    -- handlePostAction = _
    handlePostAction (PostAction pos val) = case val of
      Post postSize -> doPost postSize
      PostDead postSize -> do
        incPot postSize
        decStack pos postSize a
        Stake stakes <- use stateStakes
        mErrorAssert a (postSize >= stakes)
          . CustomError
          $ "expected postSize to be at least stakes value, "
            <> show stakes
            <> ", but found "
            <> show postSize
        streetInvestments . at pos ?= stakes
      where
        doPost :: (IsGame m b) => b -> m ()
        doPost postSize = do
          incPot postSize
          streetInvestments . at pos ?= postSize
          preuse (activeBet . _Just . amountFaced) >>= \case
            Just amountFaced'
              | postSize >= amountFaced' ->
                activeBet ?= ActionFaced PostB postSize postSize
            Just _ -> pure ()
            Nothing -> activeBet ?= ActionFaced PostB postSize postSize
          decStack pos postSize a

mErrorAssert :: IsGame m b => Action b -> Bool -> GameError b -> m ()
mErrorAssert a b e = if b then return () else throwBundleError a e

throwBundleError :: IsGame m b => Action b -> GameError b -> m a
throwBundleError a e = throwError . (\st -> GameErrorBundle e st $ Just a) =<< get

maybeToErrorBundle :: IsGame m b => Action b -> GameError b -> Maybe a -> m a
maybeToErrorBundle a e mb = case mb of
  Just a -> return a
  Nothing -> throwBundleError a e

execIsGame :: StateT s (Except e) a -> s -> Either e s
execIsGame m = runExcept . execStateT m

evalIsGame :: StateT s (ExceptT e Identity) a -> s -> Either e a
evalIsGame m = runIdentity . runExceptT . evalStateT m

testIsGame ::
  Show b =>
  StateT (GameState b) (ExceptT (GameErrorBundle b) IO) a ->
  GameState b ->
  IO ()
testIsGame actionM inputState = do
  res <- runExceptT (runStateT actionM inputState)
  case res of
    Left e -> do
      putStrLn "Game Failed"
    -- prettyPrint e
    Right (_, state') -> do
      putStrLn "*** Game completed without error ***"

prettyString :: Pretty a => a -> String
prettyString = renderString . layoutPretty defaultLayoutOptions . pretty
