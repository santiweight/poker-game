{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Poker.Game.Emulate where

import Control.Lens
  ( At (at),
    Each (each),
    Identity (runIdentity),
    non,
    preuse,
    use,
    (%=),
    (&),
    (.=),
    (<&>),
    (?=),
    _Just,
  )
import Control.Monad.Except
  ( Except,
    ExceptT,
    runExcept,
    runExceptT,
    throwError,
  )
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import qualified Data.Text as T
import Poker
import Poker.Game.AvailableActions
  ( actionMatches,
    availableActions,
  )
import Poker.Game.Types


import Prettyprinter
import Prettyprinter.Render.String
import Poker.Game.Utils

-- Monadic action to execute an Action datatype on the given state
emulateAction :: forall m b. (IsGame m b) => Action b -> m ()
emulateAction a = do
  case a of
    MkPlayerAction (PlayerAction pos actVal) -> do
      availActions <- get <&> availableActions
      use street >>= \case
        InitialTable -> throwError (PlayerActedPreDeal a)
        _ -> pure ()
      betSize <-
        flip (getBetSize pos) actVal
          =<< use (streetInvestments . at pos . non mempty)
      incPot betSize
      processActiveBet pos actVal
      processInvestment pos betSize
      decStack pos betSize
      -- TODO all non-available actions should maybe have their own errors?
      -- It would be good to silence error on each side (available/emulate)
      -- and then ensure that the same errors are covered by each.
      availActions & \case
        Left txt -> throwError . CustomError . T.unpack $ txt
        Right (availPos, availableActs) ->
          unless (any (actionMatches actVal) availableActs) $
            throwError
              . CustomError
              $ "Action "
                <> show (prettyString <$> a)
                <> "is not available\nAvailable Actions are: " <> show (availPos, availableActs)
      doRotateNextActor pos actVal
    MkDealerAction deal -> do
      emulateDeal deal
      use street >>= \case
        InitialTable -> toActQueue %= sortPreflop
        PreFlopBoard _ -> toActQueue %= sortPreflop
        _ -> do
          activeBet .= Nothing
          streetInvestments . each .= mempty
          toActQueue %= sortPostflop
    MkPostAction act -> handlePostAction act
  pure ()
  where
    getBetSize :: Position -> b -> BetAction b -> m b
    getBetSize pos previousInvestment act = do
      activeBetSize <-
        fromMaybe mempty
          <$> preuse (activeBet . _Just . amountFaced)
      case act of
        Call amount -> do
          mErrorAssert (amount `add` previousInvestment == activeBetSize) $
            CallWrongAmount previousInvestment activeBetSize a
          pure amount -- - previousInvestment
        Raise _ amountTo -> pure . fromJust $ amountTo `minus` previousInvestment
        AllInRaise _ amountTo -> pure . fromJust $ amountTo `minus` previousInvestment
        Bet amount -> pure amount
        AllIn amount -> do
          playerStack <- getStack pos
          mErrorAssert (amount == playerStack) (AllInNotFullStack previousInvestment playerStack a)
          -- mErrorAssert (amount <> streetInvestment == playerStack) (AllInNotFullStack streetInvestment playerStack a)
          pure amount
        Fold -> pure mempty
        Check -> pure mempty
    processInvestment :: (IsGame m b) => Position -> b -> m ()
    processInvestment pos betSize =
      streetInvestments . at pos . non mempty %= add betSize
    doRotateNextActor :: IsGame m b => Position -> BetAction t -> m ()
    doRotateNextActor pos =
      let doRemove = removeNextActor pos
          doRotate = rotateNextActor pos
       in \case
            Call _ -> doRotate
            Raise _ _ -> doRotate
            AllInRaise _ _ -> doRemove
            Bet _ -> doRotate
            AllIn _ -> doRemove
            Fold -> doRemove
            Check -> doRotate
    processActiveBet :: (IsGame m b) => Position -> BetAction b -> m ()
    processActiveBet pos = \case
      Call _ -> pure ()
      Raise amountBy amountTo -> setActiveBet pos amountBy amountTo
      AllInRaise amountBy amountTo -> setActiveBet pos amountBy amountTo
      Bet amount -> setActiveBet pos amount amount
      AllIn amount -> do
        faced <- fromMaybe mempty <$> preuse (activeBet . _Just . amountFaced)
        -- TODO track whether action is opened up. Action is only open if minimum raise
        -- has been reached
        if amount > faced
          then setActiveBet pos (fromJust $ amount `minus` faced) amount
          else pure ()
      Fold -> pure ()
      Check -> pure ()
    setActiveBet :: IsGame m b => Position -> b -> b -> m ()
    setActiveBet pos _ newFacedAmount =
      use activeBet >>= \case
        Nothing -> activeBet ?= ActionFaced pos newFacedAmount newFacedAmount
        Just (ActionFaced _ amountFaced' _) -> do
          raiseAmount <-
            maybeToError
              NewActionFacedLessThanPrevious
              (newFacedAmount `minus` amountFaced')
          activeBet ?= ActionFaced pos newFacedAmount raiseAmount
    handlePostAction :: (IsGame m b) => PostAction b -> m ()
    handlePostAction (PostAction pos val) = case val of
      Post postSize -> doPost pos postSize
      PostDead postSize -> do
        incPot postSize
        decStack pos postSize
        stateStakes' <- use stateStakes
        streetInvestments . at pos . non mempty %= add (minimum [postSize, _stake stateStakes'])
      PostSuperDead postSize -> do
        incPot postSize
        decStack pos postSize
      Ante amt -> do
        incPot amt
        decStack pos amt

doPost :: (IsGame m b) => Position -> b -> m ()
doPost pos' postSize = do
  incPot postSize
  streetInvestments . at pos' ?= postSize
  preuse (activeBet . _Just . amountFaced) >>= \case
    Just amountFaced'
      | postSize >= amountFaced' ->
        activeBet ?= ActionFaced pos' postSize postSize
    Just _ -> pure ()
    Nothing -> activeBet ?= ActionFaced pos' postSize postSize
  decStack pos' postSize

execIsGame :: StateT s (Except e) a -> s -> Either e s
execIsGame m = runExcept . execStateT m

evalIsGame :: StateT s (ExceptT e Identity) a -> s -> Either e a
evalIsGame m = runIdentity . runExceptT . evalStateT m

prettyString :: Pretty a => a -> String
prettyString = renderString . layoutPretty defaultLayoutOptions . pretty

emulateDeal :: IsGame m b => DealerAction -> m ()
emulateDeal deal = do
  board <- use street
  case addDealToBoard deal board of
    Nothing -> throwError $ IncorrectDeal deal board
    Just board' -> street .= board'
  where
    addDealToBoard :: DealerAction -> Board -> Maybe Board
    addDealToBoard PlayerDeal board@InitialTable = Just $ PreFlopBoard board
    addDealToBoard (FlopDeal c1 c2 c3) board@(PreFlopBoard _) =
      Just $ FlopBoard (c1, c2, c3) board
    addDealToBoard (TurnDeal card) board@(FlopBoard _ _) =
      Just $ TurnBoard card board
    addDealToBoard (RiverDeal card) board@(TurnBoard _ _) =
      Just $ RiverBoard card board
    addDealToBoard _ _ = Nothing

