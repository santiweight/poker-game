{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Poker.Game.AvailableActions where
--     notEnoughChipsErr = Left $ InvalidMove name NotEnoughChipsForAction
--     activePlayersCount = length $ getActivePlayers _players
--     chipCount = _chips $ fromJust $ getGamePlayer game name
--   where
--     NoBlind -> Left $ InvalidMove name CannotPostNoBlind
--     Small -> if chipCount < _smallBlind then notEnoughChipsErr else Right ()
--     Big -> if chipCount < _bigBlind then notEnoughChipsErr else Right ()
--   | otherwise = case blind of
--           "Cannot post blind unless a minimum of two active players are sat at table"
--         CannotPostBlind
--       InvalidMove name $
--     Left $
--   | activePlayersCount < 2 =
--   | _street /= PreDeal = Left $ InvalidMove name InvalidActionForStreet
-- canPostBlind game@Game {..} name blind
-- canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
-- An active player is one whose playerState is set to In.
-- Cannot post a blind to start a game unless at least two active players are present.
-- -- when (< 2 players state set to sat in)
-- -- Therefore the acting in turn rule wont apply for that first move
-- -- position as long as there aren't enough players sat in to start a game

-- -- | The first player to post their blinds in the predeal stage can do it from any
--     notEnoughChipsErr = Left $ InvalidMove name NotEnoughChipsForAction
--     activePlayersCount = length $ getActivePlayers _players
--     chipCount = _chips $ fromJust $ getGamePlayer game name
--   where
--     NoBlind -> Left $ InvalidMove name CannotPostNoBlind
--     Small -> if chipCount < _smallBlind then notEnoughChipsErr else Right ()
--     Big -> if chipCount < _bigBlind then notEnoughChipsErr else Right ()
--   | otherwise = case blind of
--           "Cannot post blind unless a minimum of two active players are sat at table"
--         CannotPostBlind
--       InvalidMove name $
--     Left $
--   | activePlayersCount < 2 =
--   | _street /= PreDeal = Left $ InvalidMove name InvalidActionForStreet
-- canPostBlind game@Game {..} name blind
-- canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
-- An active player is one whose playerState is set to In.


-- Cannot post a blind to start a game unless at least two active players are present.

--     notEnoughChipsErr = Left $ InvalidMove name NotEnoughChipsForAction
--     activePlayersCount = length $ getActivePlayers _players
--     chipCount = _chips $ fromJust $ getGamePlayer game name
--   where
--     NoBlind -> Left $ InvalidMove name CannotPostNoBlind
--     Small -> if chipCount < _smallBlind then notEnoughChipsErr else Right ()
--     Big -> if chipCount < _bigBlind then notEnoughChipsErr else Right ()
--   | otherwise = case blind of
--           "Cannot post blind unless a minimum of two active players are sat at table"
--         CannotPostBlind
--       InvalidMove name $
--     Left $
--   | activePlayersCount < 2 =
--   | _street /= PreDeal = Left $ InvalidMove name InvalidActionForStreet
-- canPostBlind game@Game {..} name blind
-- canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
-- An active player is one whose playerState is set to In.
-- Cannot post a blind to start a game unless at least two active players are present.
-- -- when (< 2 players state set to sat in)
-- -- Therefore the acting in turn rule wont apply for that first move
-- -- position as long as there aren't enough players sat in to start a game

-- -- | The first player to post their blinds in the predeal stage can do it from any
--     notEnoughChipsErr = Left $ InvalidMove name NotEnoughChipsForAction
--     activePlayersCount = length $ getActivePlayers _players
--     chipCount = _chips $ fromJust $ getGamePlayer game name
--   where
--     NoBlind -> Left $ InvalidMove name CannotPostNoBlind
--     Small -> if chipCount < _smallBlind then notEnoughChipsErr else Right ()
--     Big -> if chipCount < _bigBlind then notEnoughChipsErr else Right ()
--   | otherwise = case blind of
--           "Cannot post blind unless a minimum of two active players are sat at table"
--         CannotPostBlind
--       InvalidMove name $
--     Left $
--   | activePlayersCount < 2 =
--   | _street /= PreDeal = Left $ InvalidMove name InvalidActionForStreet
-- canPostBlind game@Game {..} name blind
-- canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
-- An active player is one whose playerState is set to In.


-- Cannot post a blind to start a game unless at least two active players are present.
import           Control.Lens            hiding ( Fold )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Poker.Base
import           Poker.Game.Bovada
-- -- | The first player to post their blinds in the predeal stage can do it from any
-- -- position as long as there aren't enough players sat in to start a game
-- -- Therefore the acting in turn rule wont apply for that first move
-- -- when (< 2 players state set to sat in)
import           Poker.Game.Types
import           Poker.History.Types
import           Poker.Types.BigBlind           ( SmallAmount
                                                , smallestAmountBigBlind
                                                )
import           Prettyprinter

data Ranged a = Exactly a | Between a a

data AvailableAction b = ACall b | ARaiseBetween b b | ARaiseAllIn b | AAllIn b | AFold | ACheck | ABet b b
  deriving (Show, Eq, Ord)

instance Pretty b => Pretty (AvailableAction b) where
  pretty (ACall b           ) = "ACall" <+> pretty b
  pretty (ARaiseBetween b b') = "ARaiseBetween" <+> pretty b <+> pretty b'
  pretty (ARaiseAllIn b     ) = "ARaiseAllIn" <+> pretty b
  pretty (AAllIn      b     ) = "AAllIn" <+> pretty b
  pretty AFold                = "AFold"
  pretty ACheck               = "ACheck"
  pretty (ABet b b')          = "ABet" <+> pretty b <+> pretty b'

availableActions
  :: (Num b, Ord b, SmallAmount b)
  => GameState b
  -> Either Text (Position, [AvailableAction b])
availableActions st@GameState { _potSize, _street, _stateStakes, _aggressor, _toActQueue, _posToPlayer, _streetInvestments, _activeBet }
  | length _toActQueue < 2
  = Left "No actors left"
  | (activePlayer:_) <- _toActQueue, Just activePlayer == _aggressor = Right (activePlayer, [])
  | otherwise
  = let activePlayer = head _toActQueue
    in  case _street of
          RiverBoard ca bo -> getStreetAvailableActions activePlayer st
          TurnBoard  ca bo -> getStreetAvailableActions activePlayer st
          FlopBoard  x0 bo -> getStreetAvailableActions activePlayer st
          PreFlopBoard bo  -> getStreetAvailableActions activePlayer st
          InitialTable     -> Left "InitialTable"

getStreetAvailableActions
  :: (Num b, Ord b, SmallAmount b)
  => Position
  -> GameState b
  -> Either Text (Position, [AvailableAction b])
getStreetAvailableActions activePlayer st@GameState { _activeBet, _potSize, _stateStakes }
  = case _activeBet of
    Nothing ->
      let activePlayerStack =
            st ^. posToPlayer . at activePlayer . to fromJust . stack . unStack
          betA = if activePlayerStack > getStake _stateStakes
            then ABet (getStake _stateStakes) activePlayerStack
            else AAllIn activePlayerStack
      in  Right (activePlayer, [ACheck, betA])
    Just activeBet@(ActionFaced _ amount raiseSize) ->
      let
        activePlayerStack =
          st ^. posToPlayer . at activePlayer . to fromJust . stack . unStack
        streetInv = st ^. streetInvestments . at activePlayer . non 0
        foldA     = AFold
        raiseAs   = fromMaybe []
          $ tryRaise _potSize streetInv (Stack activePlayerStack) activeBet
        callA = if activePlayerStack + streetInv < amount
          then AAllIn activePlayerStack
          else ACall (amount - streetInv)
      in
        Right (activePlayer, callA : foldA : raiseAs)
 where
  tryRaise
    :: (Num b, Ord b, SmallAmount b)
    => PotSize b
    -> b
    -> Stack b
    -> ActionFaced b
    -> Maybe [AvailableAction b]
  tryRaise (PotSize potSize) streetInv (Stack plStack) (ActionFaced _ amountFaced raiseSize)
    = let totalAvail = streetInv + plStack
          minRaise   = amountFaced + raiseSize
      in  if totalAvail < minRaise
            then if totalAvail > amountFaced
              then Just [ARaiseAllIn plStack]
              else Nothing
            else Just [ARaiseBetween minRaise plStack]


-- checkPlayerSatAtTable :: Game -> PlayerName -> Either GameErr ()
-- checkPlayerSatAtTable game@Game {..} name
--   | not atTable = Left $ NotAtTable name
--   | otherwise = Right ()
--   where
--     playerNames = getGamePlayerNames game
--     atTable = name `elem` playerNames

-- canTimeout :: PlayerName -> Game -> Either GameErr ()
-- canTimeout name game@Game {..}
--   | _street == Showdown = Left $ InvalidMove name InvalidActionForStreet
--   | otherwise = isPlayerActingOutOfTurn game name

-- makeBet :: PlayerName -> Int -> Game -> Either GameErr ()
-- makeBet name amount game@Game {..}
--   | amount < _bigBlind =
--     Left $ InvalidMove name BetLessThanb
--   | amount > chipCount =
--     Left $ InvalidMove name NotEnoughChipsForAction
--   | _street == Showdown || _street == PreDeal =
--     Left $ InvalidMove name InvalidActionForStreet
--   | _maxBet > 0 && _street /= PreFlop =
--     Left $
--       InvalidMove name $
--         CannotBetShouldRaiseInstead
--           "A bet can only be carried out if no preceding player has bet"
--   | otherwise =
--     Right ()
--   where
--     chipCount = _chips $ fromJust $ getGamePlayer game name

-- -- Keep in mind that a player can always raise all in,
-- -- even if their total chip count is less than what
-- -- a min-bet or min-raise would be.
-- canRaise :: PlayerName -> Int -> Game -> Either GameErr ()
-- canRaise name amount game@Game {..}
--   | _street == Showdown || _street == PreDeal =
--     Left $ InvalidMove name InvalidActionForStreet
--   | _street == PreFlop && _maxBet == _bigBlind =
--     Left $ InvalidMove name CannotRaiseShouldBetInstead -- a blind doesnt count as a sufficient bet to qualify a raise
--   | _maxBet == 0 =
--     Left $ InvalidMove name CannotRaiseShouldBetInstead
--   | amount < minRaise && amount /= chipCount =
--     Left $ InvalidMove name $ RaiseAmountBelowMinRaise minRaise
--   | amount > chipCount =
--     Left $ InvalidMove name NotEnoughChipsForAction
--   | otherwise =
--     Right ()
--   where
--     minRaise = 2 * _maxBet
--     chipCount = _chips $ fromJust $ getGamePlayer game name

-- canCheck :: PlayerName -> Game -> Either GameErr ()
-- canCheck name Game {..}
--   | _street == PreFlop && _committed < _bigBlind =
--     Left $
--       InvalidMove name CannotCheckShouldCallRaiseOrFold
--   | _street == Showdown || _street == PreDeal =
--     Left $
--       InvalidMove name InvalidActionForStreet
--   | _committed < _maxBet =
--     Left $
--       InvalidMove name CannotCheckShouldCallRaiseOrFold
--   | otherwise = Right ()
--   where
--     Player {..} = fromJust $ find (\Player {..} -> _playerName == name) _players

-- canFold :: PlayerName -> Game -> Either GameErr ()
-- canFold name Game {..}
--   | _street == Showdown || _street == PreDeal =
--     Left $
--       InvalidMove name InvalidActionForStreet
--   | otherwise = Right ()

-- canCall :: PlayerName -> Game -> Either GameErr ()
-- canCall name game@Game {..}
--   | _street == Showdown || _street == PreDeal =
--     Left $
--       InvalidMove name InvalidActionForStreet
--   | amountNeededToCall == 0 =
--     Left $
--       InvalidMove name CannotCallZeroAmountCheckOrBetInstead
--   | otherwise = Right ()
--   where
--     p = fromJust (getGamePlayer game name)
--     chipCount = _chips p
--     amountNeededToCall = _maxBet - _bet p

-- canSit :: Player -> Game -> Either GameErr ()
-- canSit player@Player {..} game@Game {..}
--   | _street /= PreDeal =
--     Left $
--       InvalidMove _playerName CannotSitDownOutsidePreDeal
--   | _playerName `elem` getPlayerNames _players =
--     Left $
--       AlreadySatAtTable _playerName
--   | _chips < _minBuyInChips = Left $ NotEnoughChips _playerName
--   | _chips > _maxBuyInChips = Left $ OverMaxChipsBuyIn _playerName
--   | length _players < _maxPlayers = Right ()
--   | otherwise = Left $ CannotSitAtFullTable _playerName

-- canSitOut :: PlayerName -> Game -> Either GameErr ()
-- canSitOut name game@Game {..}
--   | _street /= PreDeal = Left $ InvalidMove name CannotSitOutOutsidePreDeal
--   | isNothing currentState = Left $ NotAtTable name
--   | currentState == Just SatOut = Left $ InvalidMove name AlreadySatOut
--   | otherwise = Right ()
--   where
--     currentState = getGamePlayerState game name

-- canSitIn :: PlayerName -> Game -> Either GameErr ()
-- canSitIn name game@Game {..}
--   | _street /= PreDeal = Left $ InvalidMove name CannotSitInOutsidePreDeal
--   | isNothing currentState = Left $ NotAtTable name
--   | currentState == Just In = Left $ InvalidMove name AlreadySatIn
--   | otherwise = Right ()
--   where
--     currentState = getGamePlayerState game name

-- canLeaveSeat :: PlayerName -> Game -> Either GameErr ()
-- canLeaveSeat playerName game@Game {..}
--   | _street /= PreDeal =
--     Left $
--       InvalidMove playerName CannotLeaveSeatOutsidePreDeal
--   | playerName `notElem` getPlayerNames _players = Left $ NotAtTable playerName
--   | otherwise = Right ()

-- canJoinWaitList :: Player -> Game -> Either GameErr ()
-- canJoinWaitList player@Player {..} game@Game {..}
--   | _playerName `elem` _waitlist = Left $ AlreadyOnWaitlist _playerName
--   | otherwise = Right ()

-- validateBlindAction :: Game -> PlayerName -> Blind -> Either GameErr ()
-- validateBlindAction game@Game {..} playerName blind
--   | _street /= PreDeal =
--     Left $
--       InvalidMove playerName CannotPostBlindOutsidePreDeal
--   | otherwise = case getGamePlayer game playerName of
--     Nothing -> Left $ PlayerNotAtTable playerName
--     Just p@Player {..} -> case blindRequired of
--       Small ->
--         if blind == Small
--           then
--             if _committed >= _smallBlind
--               then Left $ InvalidMove playerName $ BlindAlreadyPosted Small
--               else Right ()
--           else Left $ InvalidMove playerName $ BlindRequired Small
--       Big ->
--         if blind == Big
--           then
--             if _committed >= bigBlindValue
--               then Left $ InvalidMove playerName $ BlindAlreadyPosted Big
--               else Right ()
--           else Left $ InvalidMove playerName $ BlindRequired Big
--       NoBlind -> Left $ InvalidMove playerName NoBlindRequired
--       where
--         blindRequired = blindRequiredByPlayer game playerName
--         bigBlindValue = _smallBlind * 2

-- validateShowOrMuckHand :: Game -> PlayerName -> Action -> Either GameErr ()
-- validateShowOrMuckHand game@Game {..} name action =
--   checkPlayerSatAtTable game name

-- -- Should Tell us if everyone has folded to the given playerName
-- -- and the hand is over
-- canShowOrMuckHand :: PlayerName -> Game -> Either GameErr ()
-- canShowOrMuckHand name game@Game {..}
--   | _street /= Showdown = Left $ InvalidMove name InvalidActionForStreet
--   | otherwise = case _winners of
--     SinglePlayerShowdown winningPlayerName ->
--       if winningPlayerName == name
--         then Right ()
--         else
--           Left $
--             InvalidMove name $
--               CannotShowHandOrMuckHand
--                 "Not winner of hand"
--     MultiPlayerShowdown _ ->
--       Left $
--         InvalidMove name $
--           CannotShowHandOrMuckHand
--             "Can only show or muck cards if winner of single player pot during showdown"
