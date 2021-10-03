{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Test.Poker.Game.Emulate where

import Control.Lens hiding (Fold)
import Control.Lens.Extras (is)
import Control.Monad.Except
  ( Except,
    MonadError,
    liftEither,
    runExcept,
  )
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Either.Extra (eitherToMaybe)
import Data.List.Extra (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( catMaybes,
    fromJust,
    mapMaybe,
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prettyprinter




import Data.Void (Void)
import Money
  ( Approximation (Floor),
    dense',
    discreteFromDense,
  )
import Poker
import Poker.Game.AvailableActions
  ( AvailableAction (..),
    actionMatches,
    availableActions,
  )
import Poker.Game.Emulate
import Poker.Game.Types
import qualified Poker.History.Bovada.Model as Bov
import qualified Poker.History.Bovada.Parser as Bov
import System.Directory
import System.FilePath
import Text.Megaparsec
import Text.Show.Pretty (ppShow)
import Poker.History.Types

testDir :: IO FilePath
testDir = getCurrentDirectory <&> (</> "test")

allHandFiles :: IO [FilePath]
allHandFiles = do
  casesDir <- (</> "test/example-handhistories/Bovada") <$> getCurrentDirectory
  listDirectory casesDir <&> fmap (casesDir </>)

parseFile ::
  FilePath ->
  IO
    ( Either
        (ParseErrorBundle Text Void)
        [Bov.History SomeBetSize]
    )
parseFile f = do
  file <- T.readFile f
  return . parse Bov.pHands [] $ file

allHands :: IO (Map FilePath [Bov.History SomeBetSize])
allHands = do
  fps <- allHandFiles
  -- print $ length fps
  results <- (\fp -> (fp,) <$> parseFile fp) `mapM` fps
  pure . Map.mapMaybe eitherToMaybe . Map.fromList $ results

makePrisms ''Action

outputAvailableActions :: IO ()
outputAvailableActions = do
  outputDir <- testDir <&> (</> "output")
  doesDirExist <- doesDirectoryExist outputDir
  when doesDirExist $ removeDirectoryRecursive outputDir
  createDirectory outputDir
  fileResults <- Map.toList <$> allHands
  forM_ fileResults $ \(fp, hands) -> do
    let fpOutputDir = outputDir </> last (splitOn "/" fp)
    createDirectory fpOutputDir
    forM_ (zip [1 :: Int ..] hands) $ \(i, hand) -> do
      let handOutputDir = fpOutputDir </> show i
      createDirectory handOutputDir
      let hand' = fmap toUsdHand hand
      outputHand
        hand'
        fp
        handOutputDir
        (Bov.gameId . Bov.header $ hand')
        (getCases hand')
  where
    toUsdHand :: SomeBetSize -> Amount "USD"
    toUsdHand (SomeBetSize USD ra) =
      unsafeMkAmount . fst . discreteFromDense Floor $ dense' ra
    toUsdHand _ = error "Unexpected non-USD hand"

data Case b = Case
  { _previousActs :: [Action b],
    _nextAct :: Action b,
    _gameState :: GameState b
  }

getCases ::
  Bov.History (Amount "USD") ->
  Either (GameErrorBundle (Amount "USD")) [Case (Amount "USD")]
getCases hand' =
  let (preflop, postflop) =
        break (is (_MkDealerAction . only PlayerDeal))
          . mapMaybe normaliseBovadaAction
          $ Bov._handActions hand'
      (preflop', postflop') =
        ( preflop `snoc` head postflop,
          filter (isn't _MkPostAction) $ tail postflop
        )
      initState = bovadaHistoryToGameState hand'
      st :: GameState (Amount "USD") =
        either (error . ppShow) id . flip runGame initState $
          mapM_
            emulateAction
            preflop'
   in fmap (Case [] (head postflop') st :) $
        runExcept
          . fmap snd
          . runWriterT
          . void
          . ($ Case [] (head postflop') st)
          $ concatM (go <$> tail postflop')
  where
    go ::
      ( IsBet b,
        Pretty b,
        Show b,
        Ord b,
        MonadWriter [Case b] m,
        MonadError (GameErrorBundle b) m
      ) =>
      Action b ->
      Case b ->
      m (Case b)
    -- TODO needs some fenceposting
    go ac (Case as nextA gs) = do
      gs' <- liftEither $ flip runGame gs $ emulateAction nextA
      tell [Case (as `snoc` nextA) ac gs']
      pure $ Case (as `snoc` nextA) ac gs'

-- TODO choose some number in the middle too
getTestActs :: IsBet b => AvailableAction b -> [BetAction b]
getTestActs (ACall b) = [Call b]
-- TODO fix raiseBy
getTestActs (ARaiseBetween b b') = [Raise mempty b, Raise mempty b']
-- TODO fix raiseBy
getTestActs (ARaiseAllIn b) = [AllInRaise mempty b]
getTestActs (AAllIn b) = [AllIn b]
getTestActs AFold = [Fold]
getTestActs ACheck = [Check]
getTestActs (ABet b b') = [Bet b, Bet b']

-- TODO choose some number in the middle too
getBadTestActs :: (IsBet b) => AvailableAction b -> [BetAction b]
getBadTestActs (ACall b) =
  [Call . fromJust $ b `minus` smallestAmount, Call $ b `add` smallestAmount]
getBadTestActs (ARaiseBetween b b') =
  -- TODO fix raiseBy amount
  [ Raise mempty . fromJust $ b `minus` smallestAmount,
    Raise mempty $ b' `add` smallestAmount
  ]
getBadTestActs (ARaiseAllIn b) =
  -- TODO fix raiseBy amount
  [ AllInRaise mempty . fromJust $ b `minus` smallestAmount,
    AllInRaise mempty $ b `add` smallestAmount
  ]
getBadTestActs (AAllIn b) =
  [AllIn . fromJust $ b `minus` smallestAmount, AllIn $ b `add` smallestAmount]
getBadTestActs AFold = []
getBadTestActs ACheck = []
getBadTestActs (ABet b b') =
  [Bet . fromJust $ b `minus` smallestAmount, Bet $ b' `add` smallestAmount]

outputHand ::
  (Show b, Ord b, IsBet b, Pretty b) =>
  Bov.History (Amount "USD") ->
  FilePath ->
  FilePath ->
  Int ->
  Either (GameErrorBundle (Amount "USD")) [Case b] ->
  IO ()
outputHand _hand originalFile handOutputDir handId cases = do
  -- let outFile = outputDir </> "out"
  case cases of
    Left geb -> print "found error" >> putStrLn (prettyString geb)
    Right x0 ->
      forM_
        (zip [1 :: Int ..] x0)
        ( \(i, Case acs nextA gs) -> do
            let _caseOutputFile = handOutputDir </> show i
            let availableActionsRes = availableActions gs
            case (nextA ^? _MkPlayerAction, availableActionsRes) of
              (Just (PlayerAction playerPos ba), Right (pos', as)) -> do
                unless (playerPos == pos' && any (actionMatches ba) as) $
                  error
                    ( ppShow
                        ( handId,
                          originalFile,
                          playerPos,
                          pos',
                          prettyString <$> ba,
                          prettyString <$> as,
                          prettyString gs
                        )
                    )
                let testActs = getTestActs =<< as
                forM_ testActs $ \testAct -> do
                  let res =
                        runGame
                          (emulateAction (MkPlayerAction (PlayerAction playerPos testAct)))
                          gs
                  case res of
                    Left geb ->
                      print "Available actions are wrong" >> print (pretty geb)
                    Right _ -> pure ()
                  pure ()
                let badTestActs = getBadTestActs =<< as
                let foo =
                      badTestActs <&> \testAct ->
                        let res =
                              runGame
                                (emulateAction (MkPlayerAction (PlayerAction playerPos testAct)))
                                gs
                         in case res of
                              Left _ -> Nothing
                              Right _ ->
                                Just $
                                  unlines
                                    [ prettyString gs,
                                      show playerPos,
                                      show $ prettyString <$> testAct
                                    ]
                case catMaybes foo of
                  [] -> pure ()
                  ls ->
                    print "This action should not have succeeded"
                      >> (putStrLn `mapM_` ls)
                pure ()
              (Just _, Left (T.unpack -> "No actors left")) -> pure ()
              (Just _, Left err) -> print "unexpected error" >> print err
              _ -> pure ()
            let _ =
                  concatWith
                    (surround (line <> line))
                    [ vsep . fmap (viaShow . fmap pretty) $ acs,
                      pretty gs,
                      either pretty pretty availableActionsRes
                    ]
            -- TODO only write some
            -- writeFile caseOutputFile . docToString $ doc
            pure ()
        )

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM = foldr (>=>) pure

runGame :: StateT s (Except e) a -> s -> Either e s
runGame m = runExcept . execStateT m

unit_output :: IO ()
unit_output = outputAvailableActions

bovadaHistoryToGameState :: IsBet b => Bov.History b -> GameState b
bovadaHistoryToGameState Bov.History {Bov._handStakes, Bov._handPlayerMap, Bov._handSeatMap, Bov._handActions, Bov._handText} =
  GameState
    { _potSize = Pot mempty,
      _street = InitialTable,
      _stateStakes = _handStakes,
      _aggressor = Nothing,
      _toActQueue = Map.keys posToPlayer',
      -- , _pastActions       = []
      -- , _futureActions     = _handActions
      _posToPlayer = posToPlayer',
      _streetInvestments = Map.empty,
      _activeBet =
        Just
          ActionFaced
            { _betType = PostB,
              _amountFaced = unStake _handStakes,
              _raiseSize = unStake _handStakes
            }
    }
  where
    posToPlayer' = Map.mapMaybe normalizePlayer _wE

    -- TODO remove unsafe fromJust
    -- Should the hands dealt to players be declared after the initialtable?
    -- Or should GameState be a data family so we can pattern match on the current street
    normalizePlayer :: Bov.Player b -> Maybe (Player b)
    normalizePlayer (Bov.Player m_ha b) =
      m_ha <&> \holding -> Player {_playerHolding = holding, _stack = Stack b}
    _wE = Map.mapMaybe (`Map.lookup` _handPlayerMap) _handSeatMap --

normaliseBovadaAction :: Bov.Action b -> Maybe (Action b)
normaliseBovadaAction (Bov.MkBetAction po ba) =
  Just $ MkPlayerAction $ PlayerAction po (normaliseBetAction ba)
normaliseBovadaAction (Bov.MkDealerAction da) =
  Just (MkDealerAction $ normaliseDealerAction da)
normaliseBovadaAction (Bov.MkTableAction ta) =
  MkPostAction <$> normaliseTableAction ta

normaliseTableAction :: Bov.TableAction b -> Maybe (PostAction b)
normaliseTableAction (Bov.KnownPlayer po tav) =
  PostAction po <$> normaliseKnownTableAction tav
normaliseTableAction (Bov.UnknownPlayer tav) = case tav of
  -- TODO make invalid states unrepresentable ;)
  Bov.Post _ -> error "Oh noes! A post action from an unknown player!"
  Bov.PostDead _ -> error "Oh noes! A post action from an unknown player!"
  _ -> Nothing

normaliseKnownTableAction ::
  Bov.TableActionValue b -> Maybe (PostActionValue b)
normaliseKnownTableAction (Bov.Post am) = Just $ Post am
normaliseKnownTableAction (Bov.PostDead am) = Just $ PostDead am
normaliseKnownTableAction _ = Nothing

normaliseDealerAction :: Bov.DealerAction -> DealerAction
normaliseDealerAction Bov.PlayerDeal = PlayerDeal
normaliseDealerAction (Bov.FlopDeal ca ca' ca2) = FlopDeal ca ca' ca2
normaliseDealerAction (Bov.TurnDeal ca) = TurnDeal ca
normaliseDealerAction (Bov.RiverDeal ca) = RiverDeal ca

normaliseBetAction :: BetAction b -> BetAction b
normaliseBetAction (Call am) = Call am
normaliseBetAction (Raise am am') = Raise am am'
normaliseBetAction (AllInRaise am am') = AllInRaise am am'
normaliseBetAction (Bet am) = Bet am
normaliseBetAction (AllIn am) = AllIn am
normaliseBetAction Fold = Fold
normaliseBetAction Check = Check
