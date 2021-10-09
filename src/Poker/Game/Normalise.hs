{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Poker.Game.Normalise where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Poker
import Poker.Game.Types
import qualified Poker.History.Bovada.Model as Bov

-- | A class to normalise a non-canonical poker game model to the canonical model
-- defined in this package.
class Normalise a b | a -> b where
  normalise :: a -> b

instance IsBet b => Normalise (Bov.History b) (GameState b) where
  normalise Bov.History {Bov._handStakes, Bov._handPlayerMap, Bov._handSeatMap, Bov._handActions, Bov._handText} =
    GameState
      { _potSize = Pot mempty,
        _street = InitialTable,
        _stateStakes = _handStakes,
        _toActQueue = Map.keys posToPlayer,
        -- , _pastActions       = []
        -- , _futureActions     = _handActions
        _posToStack = posToPlayer,
        _streetInvestments = Map.empty,
        _activeBet = Nothing
      }
    where
      posToPlayer = Map.mapMaybe normalise _wE
      _wE = Map.mapMaybe (`Map.lookup` _handPlayerMap) _handSeatMap --

      -- TODO remove unsafe fromJust
      -- Should the hands dealt to players be declared after the initialtable?
      -- Or should GameState be a data family so we can pattern match on the current street



instance Normalise (Bov.Player b) (Maybe (Stack b)) where
  normalise :: Bov.Player b -> Maybe (Stack b)
  normalise (Bov.Player m_ha b) =
    m_ha <&> \holding -> Stack b

instance Normalise (Bov.Action b) (Maybe (Action b)) where
  normalise :: Bov.Action b -> Maybe (Action b)
  normalise (Bov.MkBetAction po ba) =
    Just $ MkPlayerAction $ PlayerAction po ba
  normalise (Bov.MkDealerAction da) =
    Just (MkDealerAction $ normalise da)
  normalise (Bov.MkTableAction ta) =
    MkPostAction <$> normalise ta

instance Normalise (Bov.TableAction b) (Maybe (PostAction b)) where
  normalise ::
    Bov.TableAction b -> Maybe (PostAction b)
  normalise (Bov.KnownPlayer po tav) =
    PostAction po <$> normalise tav
  normalise (Bov.UnknownPlayer tav) = case tav of
    -- TODO make invalid states unrepresentable ;)
    Bov.Post am -> error "Oh noes! A post action from an unknown player!"
    Bov.PostDead am -> error "Oh noes! A post action from an unknown player!"
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
