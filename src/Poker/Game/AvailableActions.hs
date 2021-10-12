{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Poker.Game.AvailableActions where

-- Cannot post a blind to start a game unless at least two active players are present.
import Control.Lens hiding (Fold)
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isNothing,
  )
import Data.Text (Text)
import Poker
import Poker.Game.Types

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Control.Monad (mfilter)
#else
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String    ( renderString )
#endif

-- -- | The first player to post their blinds in the predeal stage can do it from any
-- -- position as long as there aren't enough players sat in to start a game
-- -- Therefore the acting in turn rule wont apply for that first move
-- -- when (< 2 players state set to sat in)

data AvailableAction b = ACall b | ARaiseBetween b b | ARaiseAllIn b | AAllIn b | AFold | ACheck | ABet b b
  deriving (Show, Eq, Ord)

actionMatches :: (Ord b, Eq b) => BetAction b -> AvailableAction b -> Bool
actionMatches (Call b) (ACall b') = b == b'
actionMatches (Raise _ b) (ARaiseBetween b' b'') = b >= b' && b <= b''
actionMatches (AllInRaise _ b) (ARaiseAllIn b3) = b == b3
actionMatches (AllInRaise _ b) (AAllIn b') = b == b'
actionMatches (AllInRaise _ b) (ARaiseBetween _ b'') = b == b''
actionMatches (Bet b) (ABet b' b3) = b >= b' && b <= b3
actionMatches (AllIn b) (AAllIn b') = b == b'
actionMatches (AllIn b) (ABet _ b'') = b == b''
actionMatches Fold AFold = True
actionMatches Check ACheck = True
actionMatches _ _ = False

instance Pretty b => Pretty (AvailableAction b) where
  pretty (ACall b) = "ACall" <+> pretty b
  pretty (ARaiseBetween b b') = "ARaiseBetween" <+> pretty b <+> pretty b'
  pretty (ARaiseAllIn b) = "ARaiseAllIn" <+> pretty b
  pretty (AAllIn b) = "AAllIn" <+> pretty b
  pretty AFold = "AFold"
  pretty ACheck = "ACheck"
  pretty (ABet b b') = "ABet" <+> pretty b <+> pretty b'

availableActions ::
  (Pretty b, Ord b, IsBet b) =>
  GameState b ->
  Either Text (Position, [AvailableAction b])
availableActions st@GameState {_potSize, _street, _stateStakes, _toActQueue, _posToStack, _streetInvestments, _activeBet}
  | -- TODO if a player is all in, then they are no longer in the act queue,
    -- but the game is not over!
    --  | length _toActQueue < 2
    --  = Left "No actors left"
    (activePlayer : _) <- _toActQueue,
    Just activePlayer == (_position <$> _activeBet),
    -- Not quite correct - ensure that we are not postflop
    not (activePlayer == BB && Just (unStake _stateStakes) == (_amountFaced <$> _activeBet))  =
    Right (activePlayer, [])
  | otherwise =
    let activePlayer = head _toActQueue
     in case _street of
          RiverBoard _ _ -> getStreetAvailableActions activePlayer st
          TurnBoard _ _ -> getStreetAvailableActions activePlayer st
          FlopBoard _ _ -> getStreetAvailableActions activePlayer st
          PreFlopBoard _ -> getStreetAvailableActions activePlayer st
          InitialTable -> Left "InitialTable"

getStreetAvailableActions ::
  (Pretty b, Ord b, IsBet b) =>
  Position ->
  GameState b ->
  Either Text (Position, [AvailableAction b])
getStreetAvailableActions activePlayer st@GameState {_activeBet, _potSize, _stateStakes} =
  case _activeBet of
    Nothing ->
      let activePlayerStack =
            st
              ^. posToStack
                . at activePlayer
                . to fromJust
                . to _unStack
          betA =
            if activePlayerStack > unStake _stateStakes
              then ABet (unStake _stateStakes) activePlayerStack
              else AAllIn activePlayerStack
       in Right (activePlayer, [AFold, ACheck, betA])
    Just activeBet'@(ActionFaced _ amount _) -> do
      -- TODO handle BB can check preflop
      -- TODO:
      -- Bovada Hand #3761475002 TBL#18327252 HOLDEM No Limit - 2019-04-17 09:04:56
      -- Seat 3: Dealer ($25.11 in chips)
      -- Seat 5: Big Blind [ME] ($60.99 in chips)
      -- Dealer : Set dealer [3]
      -- Dealer : Small Blind $0.10
      -- Big Blind  [ME] : Big blind $0.25
      --     *** HOLE CARDS ***
      let activePlayerStack =
            st
              ^. posToStack
                . at activePlayer
                . to fromJust
                . to _unStack
          streetInv = st ^. streetInvestments . at activePlayer . non mempty
          foldA = AFold
          raiseAs =
            fromMaybe [] $
              tryRaise _potSize streetInv (Stack activePlayerStack) activeBet'
          callA
            | activePlayerStack `add` streetInv <= amount =
              AAllIn
                activePlayerStack
            | streetInv == amount && isNothing _activeBet = ACheck
            | otherwise = maybe ACheck ACall $ mfilter (/= mempty) (amount `minus` streetInv)
      pure (activePlayer, callA : foldA : raiseAs)
  where
    tryRaise ::
      (Ord b, IsBet b) =>
      Pot b ->
      b ->
      Stack b ->
      ActionFaced b ->
      Maybe [AvailableAction b]
    -- Check that the raise is right. There's something about pot size that I'm forgetting
    -- to take into account
    tryRaise (Pot _) streetInv (Stack plStack) (ActionFaced _ amountFaced' raiseSize') =
      let totalAvail = streetInv `add` plStack
          minRaise = amountFaced' `add` raiseSize'
       in if totalAvail < minRaise
            then
              if totalAvail > amountFaced'
                then Just [ARaiseAllIn (plStack `add` streetInv)]
                else Nothing
            else Just [ARaiseBetween minRaise (streetInv `add` plStack)]
