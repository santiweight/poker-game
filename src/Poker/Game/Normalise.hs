{-# LANGUAGE NamedFieldPuns #-}
module Poker.Game.Normalise where

import Poker
import Poker.Game.Types
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))
import qualified Poker.History.Bovada.Model as Bov

bovadaHistoryToGameState :: IsBet b => Bov.History b -> GameState b
bovadaHistoryToGameState Bov.History { Bov._handStakes, Bov._handPlayerMap, Bov._handSeatMap, Bov._handActions, Bov._handText }
  = GameState
    { _potSize           = Pot mempty
    , _street            = InitialTable
    , _stateStakes       = _handStakes
    , _aggressor         = Nothing
    , _toActQueue        = Map.keys posToPlayer
              -- , _pastActions       = []
              -- , _futureActions     = _handActions
    , _posToPlayer       = posToPlayer
    , _streetInvestments = Map.empty
    , _activeBet = Just ActionFaced { _betType     = PostB
                                    , _amountFaced = unStake _handStakes
                                    , _raiseSize   = unStake _handStakes
                                    }
    }
 where
  posToPlayer = Map.mapMaybe normalizePlayer _wE

      -- TODO remove unsafe fromJust
      -- Should the hands dealt to players be declared after the initialtable?
      -- Or should GameState be a data family so we can pattern match on the current street
  normalizePlayer :: Bov.Player b -> Maybe (Player b)
  normalizePlayer (Bov.Player m_ha b) =
    m_ha <&> \holding -> Player { _playerHolding = holding, _stack = Stack b }
  _wE = Map.mapMaybe (`Map.lookup` _handPlayerMap) _handSeatMap--

normaliseBovadaAction
  :: Bov.Action b -> Maybe (Action b)
normaliseBovadaAction (Bov.MkBetAction po ba) =
  Just $ MkPlayerAction $ PlayerAction po ba
normaliseBovadaAction (Bov.MkDealerAction da) =
  Just (MkDealerAction $ normaliseDealerAction da)
normaliseBovadaAction (Bov.MkTableAction ta) =
  MkPostAction <$> normaliseTableAction ta

normaliseTableAction
  :: Bov.TableAction b -> Maybe (PostAction b)
normaliseTableAction (Bov.KnownPlayer po tav) =
  PostAction po <$> normaliseKnownTableAction tav
normaliseTableAction (Bov.UnknownPlayer tav) = case tav of
  -- TODO make invalid states unrepresentable ;)
  Bov.Post     am -> error "Oh noes! A post action from an unknown player!"
  Bov.PostDead am -> error "Oh noes! A post action from an unknown player!"
  _               -> Nothing

normaliseKnownTableAction
  :: Bov.TableActionValue b
  -> Maybe (PostActionValue b)
normaliseKnownTableAction (Bov.Post     am) = Just $ Post am
normaliseKnownTableAction (Bov.PostDead am) = Just $ PostDead am
normaliseKnownTableAction _                 = Nothing

normaliseDealerAction :: Bov.DealerAction -> DealerAction
normaliseDealerAction Bov.PlayerDeal            = PlayerDeal
normaliseDealerAction (Bov.FlopDeal ca ca' ca2) = FlopDeal ca ca' ca2
normaliseDealerAction (Bov.TurnDeal  ca       ) = TurnDeal ca
normaliseDealerAction (Bov.RiverDeal ca       ) = RiverDeal ca
