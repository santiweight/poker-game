{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Poker.Game.Types where

import           Control.Lens                   ( Bifunctor(bimap)
                                                , makeLenses
                                                , makePrisms
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc      ( Pretty(pretty) )
import           GHC.Generics                   ( Generic )
import           Poker.Base                     ( Action
                                                , BetAction
                                                , BigBlind
                                                , Board
                                                , DealerAction
                                                , Hand
                                                , Position
                                                , PotSize
                                                , Seat
                                                , Stake
                                                )
import           Prettyprinter                  ( Pretty )
import           Prettyprinter

newtype BigBlinds = BigBlinds Double deriving (Show, Eq, Ord)

deriving instance Num BigBlinds

newtype Stack b = Stack { _unStack :: b} deriving (Show, Eq, Ord, Generic, Functor, Pretty)

makeLenses ''Stack

data Player t = Player
  { _playerHolding :: !Hand
  , _stack         :: !(Stack t) -- TODO use newtype
  }
  deriving (Show, Eq, Ord, Generic, Functor)

instance Pretty t => Pretty (Player t) where
  pretty (Player ha st) = pretty ha <+> pretty st

makeLenses ''Player


data GameState g = GameState
  { _potSize           :: PotSize g
  , _street            :: Board
  , _stateStakes       :: Stake g
  -- , _gameID              :: Int
  -- , _stateHandText       :: String
  , _aggressor         :: Maybe Position
  -- , _lastStreetAggressor :: Maybe Position
                           -- The following data members should stay in GameState
  , _toActQueue        :: [Position]
  -- , _pastActions         :: [Action g]
  -- , _futureActions       :: [Action g]
  , _posToPlayer       :: Map Position (Player g)
  , _streetInvestments :: Map Position g
  , _activeBet         :: Maybe (ActionFaced g)
  }
deriving instance Show a => Show (GameState a)
deriving instance Eq a => Eq (GameState a)
deriving instance Functor GameState


instance Pretty b => Pretty (GameState b) where
  pretty GameState { _potSize, _street, _stateStakes, _aggressor, _toActQueue, _posToPlayer, _streetInvestments, _activeBet }
    = concatWith (\a b -> a <> line <> b)
      [ "Stakes:" <+> pretty _stateStakes
      , "Investments:" <+> pretty (Map.toList _streetInvestments)
      , "Street:" <+> pretty _street
      , hang 4 $ "Stacks:" <> line <>
        (vsep . fmap (asTuple . bimap pretty pretty) $ Map.toList _posToPlayer)
      , "Queue: " <> pretty (show _toActQueue)
      , "ActiveBet:" <+> (viaShow . fmap pretty) _activeBet
      , "Potsize: " <> pretty _potSize
      , "Aggressor: " <> viaShow _aggressor
      ]
    where asTuple = \(a, b) -> "(" <> a <> "," <> b <> ")"
data ActionFaced t = ActionFaced
  { _betType     :: BetType
  , _amountFaced :: t
  , _raiseSize   :: t
  }
  deriving (Show, Read, Ord, Eq, Functor)

instance Pretty b => Pretty (ActionFaced b) where
  pretty ActionFaced { _betType, _amountFaced, _raiseSize } = hsep
    [ "ActionFaced: "
    , "betType: " <> viaShow _betType
    , "amountFaced: " <> pretty _amountFaced
    , "raiseSize: " <> pretty _raiseSize
    ]

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

instance Pretty g => Pretty (GameError g) where
  pretty = viaShow . fmap pretty

data GameErrorBundle g = GameErrorBundle
  { _bundleError :: GameError g
  , _bundleState :: GameState g
  , _bundleCausingAction :: Action g
  }

instance Pretty g => Pretty (GameErrorBundle g) where
  pretty (GameErrorBundle ge gs ac) = hang 4 $ vsep ["GameErrorBundle:", pretty ge, pretty gs, viaShow . fmap pretty $ ac]

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
