{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Poker.Game.Normalise where

import Control.Applicative (liftA2)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Poker
import Poker.Game.Types
import qualified Poker.History.Base as Model
import qualified Poker.History.Bovada.Model as Bov
import qualified Poker.History.PokerStars.Model as PS
import Data.Function
import qualified Data.Text as T
import Poker.Game (smallBlindPosition)

-- | A class to normalise a non-canonical poker game model to the canonical model
-- defined in this package.
class Normalise a b | a -> b where
  normalise :: a -> b

instance IsBet b => Normalise (Bov.History b) (GameState b) where
  normalise Bov.History {Bov.header, Bov._handStakes, Bov._handPlayerMap, Bov._handSeatMap, Bov._handActions, Bov._handText} =
    GameState
      { _potSize = Pot mempty,
        _street = InitialTable,
        _stateStakes = _handStakes,
        _toActQueue = Map.keys normedPosToStack,
        _posToStack = normedPosToStack, -- if Map.size normedPosToStack == numPlayers then normedpostostack else error (show (header, posToStack, normedPosToStack, numPlayers, num)),
        _streetInvestments = Map.empty,
        _activeBet = Nothing
      }
    where
      normalisePos = normalise . (num,)
      numPlayers = Map.size _wE
      num = fromMaybe (error $ "Unsupported number of players: " <> show numPlayers) $ mkNumPlayers numPlayers
      normedPosToStack = Map.mapKeys normalisePos posToStack
      posToStack = Map.mapMaybe normalise _wE
      _wE = Map.mapMaybe (`Map.lookup` _handPlayerMap) _handSeatMap --

instance Normalise (Bov.Player b) (Maybe (Stack b)) where
  normalise :: Bov.Player b -> Maybe (Stack b)
  normalise (Bov.Player m_ha b) =
    m_ha <&> \_ -> Stack b

instance Normalise (NumPlayers, Bov.Action b) (Maybe (Action b)) where
  normalise :: (NumPlayers, Bov.Action b) -> Maybe (Action b)
  normalise (num, Bov.MkBetAction po ba) =
    Just $ MkPlayerAction $ PlayerAction (normalise (num, po)) ba
  normalise (_, Bov.MkDealerAction da) =
    Just (MkDealerAction $ normalise da)
  normalise (num, Bov.MkTableAction ta) =
    MkPostAction <$> normalise (num, ta)

instance Normalise (NumPlayers, Bov.TableAction b) (Maybe (PostAction b)) where
  normalise ::
    (NumPlayers, Bov.TableAction b) -> Maybe (PostAction b)
  normalise (num, Bov.KnownPlayer po tav) =
    PostAction (normalise (num, po)) <$> normalise tav
  normalise (_, Bov.UnknownPlayer tav) = case tav of
    -- TODO make invalid states unrepresentable ;)
    Bov.Post _ -> error "Oh noes! A post action from an unknown player!"
    Bov.PostDead _ -> error "Oh noes! A post action from an unknown player!"
    _ -> Nothing

instance Normalise (Bov.TableActionValue b) (Maybe (PostActionValue b)) where
  normalise :: Bov.TableActionValue b -> Maybe (PostActionValue b)
  normalise (Bov.Post am) = Just $ Post am
  normalise (Bov.PostDead am) = Just $ PostDead am
  normalise _ = Nothing

instance Normalise Bov.DealerAction DealerAction where
  normalise Bov.PlayerDeal = PlayerDeal
  normalise (Bov.FlopDeal ca ca' ca2) = FlopDeal ca ca' ca2
  normalise (Bov.TurnDeal ca) = TurnDeal ca
  normalise (Bov.RiverDeal ca) = RiverDeal ca

instance IsBet b => Normalise (PS.History b) (GameState b) where
  normalise PS.History {PS._handStakes, PS._handPlayerMap, PS._handSeatMap, PS._handActions, PS._handText} =
    GameState
      { _potSize = Pot mempty,
        _street = InitialTable,
        _stateStakes = _handStakes,
        _toActQueue = normalisePos . fromJust . flip Map.lookup _handSeatMap <$> Map.keys _handSeatMap,
        -- , _pastActions       = []
        -- , _futureActions     = _handActions
        _posToStack =
          Map.mapKeys normalisePos $
            Map.fromList $
              Map.assocs _handPlayerMap <&> (\(seat, PS.Player _ b) -> (findPos seat, Stack b)),
        _streetInvestments = Map.empty,
        _activeBet = Nothing
      }
    where
      normalisePos = id
      num = fromJust . mkNumPlayers $ Map.size _handPlayerMap
      findPos = fromJust . flip Map.lookup _handSeatMap

instance Normalise (PS.Player b) (Maybe (Stack b)) where
  normalise :: PS.Player b -> Maybe (Stack b)
  normalise (PS.Player _ b) = Just $ Stack b

instance Normalise (PS.Action b) (Maybe (Action b)) where
  normalise :: PS.Action b -> Maybe (Action b)
  normalise (PS.MkBetAction po ba) =
    Just $ MkPlayerAction $ PlayerAction po ba
  normalise (PS.MkDealerAction da) =
    Just (MkDealerAction $ normalise da)
  normalise (PS.MkTableAction ta) =
    MkPostAction <$> normalise ta

instance Normalise (PS.TableAction b) (Maybe (PostAction b)) where
  normalise ::
    PS.TableAction b -> Maybe (PostAction b)
  normalise (PS.KnownPlayer po tav) =
    PostAction po <$> normalise tav
  normalise (PS.UnknownPlayer tav) = case tav of
    -- TODO make invalid states unrepresentable ;)
    PS.Post _ -> error "Oh noes! A post action from an unknown player!"
    _ -> Nothing

instance Normalise (PS.TableActionValue b) (Maybe (PostActionValue b)) where
  normalise :: PS.TableActionValue b -> Maybe (PostActionValue b)
  normalise (PS.Post am) = Just $ Post am
  normalise (PS.PostDead am) = Just $ PostDead am
  normalise (PS.PostSuperDead am) = Just $ PostSuperDead am
  normalise (PS.Ante am) = Just $ Ante am
  normalise _ = Nothing

instance Normalise PS.DealerAction DealerAction where
  normalise PS.PlayerDeal = PlayerDeal
  normalise (PS.FlopDeal ca ca' ca2) = FlopDeal ca ca' ca2
  normalise (PS.TurnDeal ca) = TurnDeal ca
  normalise (PS.RiverDeal ca) = RiverDeal ca

-- >>> import Poker
-- >>> normalise . (\a -> (FivePlayers,a)) <$> Model.allPositions
instance Normalise (NumPlayers, Model.Position) Position where
  normalise (np, pos) = case np of
    TwoPlayers -> case pos of
      Model.BU -> buttonPosition np
      Model.BB -> bigBlindPosition np
      _ -> err
    _ -> case pos of
      Model.BU -> buttonPosition np
      Model.SB -> smallBlindPosition np
      Model.BB -> bigBlindPosition np
      Model.UTG -> allPositions np !! 0
      Model.UTG1 -> allPositions np !! 1
      Model.UTG2 -> allPositions np !! 2
      Model.UTG3 -> allPositions np !! 3
      Model.UTG4 -> allPositions np !! 4
      Model.UTG5 -> allPositions np !! 5

    where
      err = error $ "Invalid position/number of players combo: " <> show (np, pos)
      normMap = allPositions np & (Map.fromList <$> liftA2 zip (fmap (positionToTxt np)) id)
