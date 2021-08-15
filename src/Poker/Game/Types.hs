{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}

module Poker.Game.Types where

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )
import           Data.Map                       ( Map )
import           Poker.Base                     ( Action
                                                , BetAction
                                                , Board
                                                , DealerAction
                                                , Hand
                                                , Position
                                                , PotSize
                                                , Seat
                                                , Stake, BigBlind
                                                )
import GHC.Generics (Generic)

newtype BigBlinds = BigBlinds Double deriving (Show, Eq, Ord)

deriving instance Num BigBlinds

newtype Stack b = Stack { _unStack :: b} deriving (Show, Eq, Ord, Generic, Functor)

makeLenses ''Stack

data Player t = Player
  { _playerHolding  :: !Hand
  , _stack          :: !(Stack t) -- TODO use newtype
  }
  deriving (Show, Eq, Ord, Generic, Functor)

makeLenses ''Player


data GameState g = GameState
  { _potSize             :: PotSize g
  , _street              :: Board
  , _stateStakes         :: Stake g
  , _gameID              :: Int
  , _stateHandText       :: String
  , _aggressor           :: Maybe Position
  , _lastStreetAggressor :: Maybe Position
                           -- The following data members should stay in GameState
  , _toActQueue          :: [Position]
  , _pastActions         :: [Action g]
  , _futureActions       :: [Action g]
  , _posToPlayer         :: Map Position (Player g)
  , _streetInvestments   :: Map Position g
  , _activeBet           :: Maybe (ActionFaced g)
  }
deriving instance Show a => Show (GameState a)
deriving instance Eq a => Eq (GameState a)
deriving instance Functor GameState

data ActionFaced t = ActionFaced
  { _betType   :: BetType
  , _amountFaced   :: t
  , _raiseSize :: t
  }
  deriving (Show, Read, Ord, Eq, Functor)

data BetType = PostB | OneB | TwoB | ThreeB | FourB
             | FiveB | SixBet | SevenBet | EightBet
             | NineBet | TenBet | ElevenBet | TwelveBet
             | ThirtBet | FourtB | FiftBet | SixtBet
             | SeventBet | EighttB | NinetBet
  deriving (Show, Read, Ord, Eq, Enum)

data GameError g = PlayerNotFound
               | SeatNotFound Position (Action g)
               | NegativePlayerStack (Action g)
               | IncorrectDeal DealerAction Board
               | PlayerActedPreDeal { _badAct :: Action g }
               | CallWrongAmount { _expected :: g, _badAct :: Action g }
               | NegativePotSize
               | WrongPlayerActed { _expectedPosition :: Position
                                  , _actualPosition :: Position
                                  }
                                  | AllInNotFullStack { _stackSize :: g, _badAllInAct :: Action g}
                                  | ActedPreDeal
               | NoPlayersInQueue
               | CustomError String
  deriving (Show, Eq, Functor)

data GameErrorBundle g = GameErrorBundle
  { _bundleError :: GameError g
  , _bundleState :: GameState g
  }

deriving instance Show a => Show (GameErrorBundle a)
deriving instance Eq a => Eq (GameErrorBundle a)
deriving instance Functor GameErrorBundle

data EvalState t = EvalState
  { _nextActions :: [Action t]
  , _accRanges   :: Map String (ActionRange t)
  , _handState   :: GameState t
  }

type ActionRange t = Map Hand [BetAction t]

makeLenses 'EvalState
makeLenses ''GameError
makeLenses ''ActionFaced
makePrisms ''GameError
makeLenses ''GameErrorBundle
makeLenses ''GameState
