{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.Game.Bovada where

import           Control.Arrow                  ( (>>>) )
import           Control.Lens                   ( (%=)
                                                , (+=)
                                                , (-=)
                                                , (.=)
                                                , (<%=)
                                                , (<&>)
                                                , (<<.=)
                                                , (?=)
                                                , At(at)
                                                , Each(each)
                                                , Identity(runIdentity)
                                                , Ixed(ix)
                                                , Traversal'
                                                , _Just
                                                , makeLenses
                                                , mapped
                                                , non
                                                , preuse
                                                , uncons
                                                , use
                                                , view, (&)
                                                )
import           Control.Monad.Except           ( Except
                                                , ExceptT
                                                , MonadError
                                                , runExcept
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import qualified Data.Text                     as T
import           Debug.Trace
import           Poker.Base
import           Poker.Game.AvailableActions    ( actionMatches
                                                , availableActions
                                                )
import           Poker.Game.Types
import           Prettyprinter                  ( Pretty (pretty), layoutPretty, defaultLayoutOptions )
import Prettyprinter.Render.String

type IsGame m b
  = ( SmallAmount b
    , Pretty b
    , Show b
    , Num b
    , Ord b
    , MonadState (GameState b) m
    , MonadError (GameErrorBundle b) m
    )


incPot :: (Num b, MonadState (GameState b) m) => b -> m ()
incPot bet = potSize . mapped += bet

getPlayer :: MonadReader (GameState b) m => Position -> m (Maybe (Player b))
getPlayer pos_ = ask <&> view (posToPlayer . at pos_)

-- Reduce state pot size
decPot :: (IsGame m b) => Action b -> b -> m (PotSize b)
decPot a amount = do
  checkPotSize <- potSize <%= dec amount
  mErrorAssert a (checkPotSize >= PotSize 0) NegativePotSize
  return checkPotSize
 where
  dec :: Num b => b -> PotSize b -> PotSize b
  dec betSize (PotSize potSize_) = PotSize $ potSize_ - betSize

-- Increase stack size at a seat position
incStack :: (IsGame m b) => Position -> b -> Action b -> m ()
incStack pos amount a = do
  _ <- getStack a pos
  -- mErrorAssert (playerStack - amount > 0) (NegativePlayerStack badAct)
  atPlayerStack pos -= amount

-- Decrease stack size at a seat position
decStack
  :: (IsGame m b, MonadState (GameState t) m)
  => Position
  -> b
  -> Action b
  -> m ()
decStack pos amount badAct = do
  playerStack <- getStack badAct pos
  mErrorAssert badAct (playerStack - amount >= 0) (NegativePlayerStack badAct)
  atPlayerStack pos -= amount

atPlayerStack :: Position -> Traversal' (GameState t) t
atPlayerStack pos = posToPlayer . ix pos . stack . unStack

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
    Nothing     -> throwBundleError a $ IncorrectDeal deal board
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
emulateAction :: forall m b . (IsGame m b) => Action b -> m ()
emulateAction a = do
  res <- case a of
    MkPlayerAction pa@(PlayerAction pos actVal _) -> do
      availActions <- get <&> availableActions

      use street >>= \case
        InitialTable -> throwBundleError a (PlayerActedPreDeal a)
        _            -> pure ()
      betSize <- flip (getBetSize pos) actVal
        =<< use (streetInvestments . at pos . non 0)
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
                unless (any (actionMatches actVal) availableActions)
                  $  throwBundleError a
                  .  CustomError
                  $  "Action "
                  <> show (prettyString<$> a)
                  <> "is not available"

      doRotateNextActor pos actVal
    MkDealerAction deal -> do
      addDealToState a deal
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
  -- when (any actionMatches )
  pure ()
 where
  saveAggressor :: IsGame m b => m ()
  saveAggressor = aggressor .= Nothing
  -- saveAggressor :: IsGame m => m ()
  -- saveAggressor = (lastStreetAggressor .=) =<< aggressor <<.= Nothing
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
  getBetSize :: Position -> b -> BetAction b -> m b
  getBetSize pos previousInvestment act = do
    activeBet <- fromMaybe 0 <$> preuse (activeBet . _Just . amountFaced)
    case act of
      Call amount -> do
        mErrorAssert a (amount == activeBet - previousInvestment)
          $ CallWrongAmount activeBet a
        pure amount -- - previousInvestment
      Raise      _ amountTo -> pure $ amountTo - previousInvestment
      AllInRaise _ amountTo -> pure $ amountTo - previousInvestment
      Bet   amount          -> pure amount
      AllIn amount          -> do
        playerStack <- getStack a pos
        mErrorAssert a (amount == playerStack) (AllInNotFullStack playerStack a)
        pure amount
      Fold        -> pure 0
      Check       -> pure 0
      OtherAction -> pure 0
  processInvestment :: (IsGame m b) => Position -> b -> m ()
  processInvestment pos betSize =
    streetInvestments . at pos . non 0 %= (+ betSize)
  doRotateNextActor :: IsGame m b => Position -> BetAction t -> m ()
  doRotateNextActor pos =
    let doRemove = removeNextActor a pos
        doRotate = rotateNextActor a pos
    in  \case
          Call _         -> doRotate
          Raise      _ _ -> doRotate
          AllInRaise _ _ -> doRemove
          Bet   _        -> doRotate
          AllIn _        -> doRemove
          Fold           -> doRemove
          Check          -> doRotate
          OtherAction    -> pure () -- TODO
  processActiveBet :: (IsGame m b) => BetAction b -> m ()
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
  incActiveBet :: IsGame m b => b -> m ()
  incActiveBet newFacedAmount = use activeBet >>= \case
    Nothing -> activeBet ?= ActionFaced OneB newFacedAmount newFacedAmount
    Just (ActionFaced bType amountFaced _) -> activeBet ?= ActionFaced
      (succ bType)
      newFacedAmount
      (newFacedAmount - amountFaced)
  handleTableAction :: (IsGame m b) => TableAction b -> m ()
  handleTableAction UnknownAction         = pure () -- FIXME
  handleTableAction (TableAction pos val) = case val of
    Deposit  _        -> return () -- TODO may need to inc stack size
    Post     postSize -> doPost postSize
    PostDead postSize -> do
      incPot postSize
      decStack pos postSize a
      Stake stakes <- use stateStakes
      mErrorAssert a (postSize >= stakes)
        .  CustomError
        $  "expected postSize to be at least stakes value, "
        <> show stakes
        <> ", but found "
        <> show postSize
      streetInvestments . at pos ?= stakes
    Enter -> return () -- Not sure what this should be
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
      _ <- decPot a amount
      incStack pos amount a
    Result _ -> return () -- TODO
   where
    doPost :: (IsGame m b) => b -> m ()
    doPost postSize = do
      incPot postSize
      streetInvestments . at pos ?= postSize
      preuse (activeBet . _Just . amountFaced) >>= \case
        Just amountFaced' | postSize >= amountFaced' ->
          activeBet ?= ActionFaced PostB postSize postSize
        Just _  -> pure ()
        Nothing -> activeBet ?= ActionFaced PostB postSize postSize
      decStack pos postSize a

mErrorAssert :: IsGame m b => Action b -> Bool -> GameError b -> m ()
mErrorAssert a b e = if b then return () else throwBundleError a e

throwBundleError :: IsGame m b => Action b -> GameError b -> m a
throwBundleError a e = throwError . (\st -> GameErrorBundle e st a) =<< get

maybeToErrorBundle :: IsGame m b => Action b -> GameError b -> Maybe a -> m a
maybeToErrorBundle a e mb = case mb of
  Just a  -> return a
  Nothing -> throwBundleError a e

{- Testing and Printing methods -}

execIsGame :: StateT s (Except e) a -> s -> Either e s
execIsGame m = runExcept . execStateT m

evalIsGame :: StateT s (ExceptT e Identity) a -> s -> Either e a
evalIsGame m = runIdentity . runExceptT . evalStateT m

-- testIsGame
--   :: MonadIO m  =>
--      StateT GameState (ExceptT e m) a -> GameState -> m (Either e String)
testIsGame
  :: Show b
  => StateT (GameState b) (ExceptT (GameErrorBundle b) IO) a
  -> GameState b
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
      -- putStr $ view stateHandText state'


prettyString :: Pretty a => a -> String
prettyString = renderString . layoutPretty defaultLayoutOptions . pretty