{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Poker.Game.Types where

import Control.Lens
  ( Bifunctor (bimap),
    makeLenses,
    makePrisms,
  )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif
import Poker

data PlayerAction t = PlayerAction {_pos :: !Position, _action :: !(BetAction t)}
  deriving (Read, Show, Eq, Ord, Functor)

-- TODO remove post/dead separation here:
-- data PostAction t = PostAction !Position !t
data PostActionValue t
  = Post !t
  | PostDead !t
  | PostSuperDead !t
  | Ante !t
  deriving (Read, Show, Ord, Eq, Functor)

data PostAction t = PostAction !Position !(PostActionValue t)
  deriving (Read, Show, Eq, Ord, Functor)

-- TODO Fix the below to become the above
data DealerAction
  = PlayerDeal
  | FlopDeal !Card !Card !Card
  | TurnDeal !Card
  | RiverDeal !Card
  deriving (Read, Show, Eq, Ord)

data Action t
  = MkPlayerAction !(PlayerAction t)
  | MkDealerAction !DealerAction
  | MkPostAction !(PostAction t)
  deriving (Read, Show, Eq, Ord, Functor)

newtype BigBlinds = BigBlinds Double deriving (Show, Eq, Ord)

deriving instance Num BigBlinds

data GameState g = GameState
  { _potSize :: Pot g,
    _street :: Board,
    _stateStakes :: Stake g,
    _toActQueue :: [Position],
    _posToStack :: Map Position (Stack g),
    _streetInvestments :: Map Position g,
    _activeBet :: Maybe (ActionFaced g)
  }

deriving instance Show a => Show (GameState a)

deriving instance Eq a => Eq (GameState a)

deriving instance Functor GameState

instance Pretty b => Pretty (GameState b) where
  pretty GameState {_potSize, _street, _stateStakes, _toActQueue, _posToStack, _streetInvestments, _activeBet} =
    concatWith
      (\a b -> a <> line <> b)
      [ "Stakes:" <+> pretty _stateStakes,
        "Investments:" <+> pretty (Map.toList _streetInvestments),
        "Street:" <+> viaShow _street,
        hang 4 $
          "Stacks:"
            <> line
            <> ( vsep . fmap (asTuple . bimap pretty pretty) $
                   Map.toList
                     _posToStack
               ),
        "Queue: " <> pretty (show _toActQueue),
        "ActiveBet:" <+> (viaShow . fmap pretty) _activeBet,
        "Potsize: " <> pretty _potSize
      ]
    where
      asTuple = \(a, b) -> "(" <> a <> "," <> b <> ")"

data ActionFaced t = ActionFaced
  { _position :: Position,
    _amountFaced :: t,
    _raiseSize :: t
  }
  deriving (Show, Read, Ord, Eq, Functor)

instance Pretty b => Pretty (ActionFaced b) where
  pretty ActionFaced {_amountFaced, _raiseSize} =
    hsep
      [ "ActionFaced: ",
        "amountFaced: " <> pretty _amountFaced,
        "raiseSize: " <> pretty _raiseSize
      ]

data BetType
  = OneB
  | TwoB
  | ThreeB
  | FourB
  | FiveB
  | SixBet
  | SevenBet
  | EightBet
  | NineBet
  | TenBet
  | ElevenBet
  | TwelveBet
  | ThirteenBet
  | FourteenBet
  | FifteenBet
  | SixteenBet
  | SeventeenBet
  | EighteenBet
  | NineteenBet
  deriving (Show, Read, Ord, Eq, Enum)

data GameError g
  = PlayerNotFound
  | SeatNotFound Position (Action g)
  | NegativePlayerStack
  | IncorrectDeal DealerAction Board
  | PlayerActedPreDeal {_badAct :: Action g}
  | CallWrongAmount {_streetInvestment :: g, _expected :: g, _badAct :: Action g}
  | NegativePotSize
  | WrongPlayerActed
      { _expectedPosition :: Position,
        _actualPosition :: Position
      }
  | AllInNotFullStack {_streetInvestment :: g, _stackSize :: g, _badAllInAct :: Action g}
  | ActedPreDeal
  | NoPlayersInQueue
  | NewActionFacedLessThanPrevious
  | CustomError String
  deriving (Show, Eq, Functor)

instance Pretty g => Pretty (GameError g) where
  pretty = viaShow . fmap pretty

data EvalState t = EvalState
  { _nextActions :: [Action t],
    _accRanges :: Map String (ActionRange t),
    _handState :: GameState t,
    _holdings :: Map Position Hole,
    _activeBetType :: Maybe BetType
  }
  deriving (Show)

type ActionRange t = Map Hole [BetAction t]

makeLenses 'EvalState
makeLenses ''GameError
makeLenses ''ActionFaced
makePrisms ''GameError
makePrisms ''Board
makePrisms ''Action
makePrisms ''DealerAction
makeLenses ''GameState
