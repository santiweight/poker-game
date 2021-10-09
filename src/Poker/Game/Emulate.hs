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
import Poker.Game.Utils
import Poker.Utils (prettyText)
#else
import           Data.Text.Prettyprint.Doc ( Pretty(pretty)
                                                , defaultLayoutOptions
                                                , layoutPretty
                                                )
import           Data.Text.Prettyprint.Doc.Render.String
#endif

emulateDeal :: IsGame m b => Action b -> DealerAction -> m ()
emulateDeal a deal = do
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

-- Monadic action to execute an Action datatype on the given state
emulateAction :: forall m b. (IsGame m b) => Action b -> m ()
emulateAction a = do
  res <- case a of
    MkPlayerAction pa@(PlayerAction pos actVal) -> do
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
      decStack pos betSize a
      -- TODO all non-available actions should maybe have their own errors?
      -- It would be good to silence error on each side (available/emulate)
      -- and then ensure that the same errors are covered by each.
      availActions & \case
        Left txt -> throwError . CustomError . T.unpack $ txt
        Right (pos, availableActions) ->
          unless (any (actionMatches actVal) availableActions) $
            throwError
              . CustomError
              $ "Action "
                <> show (prettyText <$> a)
                <> "is not available"
      doRotateNextActor pos actVal
    MkDealerAction deal -> do
      emulateDeal a deal
      activeBet .= Nothing
      use street >>= \case
        InitialTable -> toActQueue %= sortPreflop
        PreFlopBoard _ -> toActQueue %= sortPreflop
        _ -> do
          streetInvestments . each .= mempty
          toActQueue %= sortPostflop
    MkPostAction act -> handlePostAction act
  pure ()
  where
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
          mErrorAssert (amount `add` previousInvestment == activeBetSize) $
            CallWrongAmount activeBetSize a
          pure amount -- - previousInvestment
        Raise amountBy _ -> pure amountBy
        AllInRaise amountBy _ -> pure amountBy
        Bet amount -> pure amount
        AllIn amount -> do
          playerStack <- getStack a pos
          mErrorAssert (amount == playerStack) (AllInNotFullStack playerStack a)
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
    setActiveBet pos raiseBy newFacedAmount =
      use activeBet >>= \case
        Nothing -> activeBet ?= ActionFaced pos newFacedAmount newFacedAmount
        Just (ActionFaced _ amountFaced _) -> do
          raiseAmount <-
            maybeToError
              NewActionFacedLessThanPrevious
              (newFacedAmount `minus` amountFaced)
          activeBet ?= ActionFaced pos newFacedAmount raiseAmount
    handlePostAction :: (IsGame m b) => PostAction b -> m ()
    -- handlePostAction = _
    handlePostAction (PostAction pos val) = case val of
      Post postSize -> doPost pos postSize
      PostDead postSize -> do
        incPot postSize
        decStack pos postSize a
        Stake stakes <- use stateStakes
        mErrorAssert (postSize >= stakes)
          . CustomError
          $ "expected postSize to be at least stakes value, "
            <> show stakes
            <> ", but found "
            <> show postSize
        streetInvestments . at pos ?= stakes
      where
        doPost :: (IsGame m b) => Position -> b -> m ()
        doPost pos postSize = do
          incPot postSize
          streetInvestments . at pos ?= postSize
          preuse (activeBet . _Just . amountFaced) >>= \case
            Just amountFaced'
              | postSize >= amountFaced' ->
                activeBet ?= ActionFaced pos postSize postSize
            Just _ -> pure ()
            Nothing -> activeBet ?= ActionFaced pos postSize postSize
          decStack pos postSize a

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
