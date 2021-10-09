{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Test.Poker.Game.Emulate where

import Control.Lens hiding (Fold)
import Control.Lens.Extras (is)
import Control.Monad.Except
  ( Except,
    MonadError,
    liftEither,
    mapExceptT,
    runExcept,
    withExceptT,
  )
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Either.Extra (eitherToMaybe, mapLeft)
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
import Data.Void (Void)
import GHC.Stack (HasCallStack)
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
import Poker.Game.Normalise (Normalise (normalise))
import Poker.Game.Types
import qualified Poker.History.Bovada.Model as Bov
import qualified Poker.History.Bovada.Parser as Bov
import Poker.History.Types
import Prettyprinter
import System.Directory
import System.FilePath hiding (normalise)
import Text.Megaparsec
import Text.Show.Pretty (pPrint, ppShow)

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

unit_output :: IO ()
unit_output = outputAvailableActions

outputAvailableActions :: IO ()
outputAvailableActions = do
  fileResults <- Map.toList <$> Test.Poker.Game.Emulate.allHands
  print $ "Testing " <> show (sum $ length . snd <$> fileResults) <> " hands"
  forM_ fileResults $ \(fp, hands) -> do
    forM_ hands $ \hand -> do
      let hand' = fmap toUsdHand hand
      outputHand
        hand'
        fp
        (Bov.gameId . Bov.header $ hand')
        (getCases hand')
  where
    toUsdHand :: SomeBetSize -> Amount "USD"
    toUsdHand (SomeBetSize USD ra) =
      unsafeMkAmount . fst . discreteFromDense Floor $ dense' ra
    toUsdHand _ = error "Unexpected non-USD hand"

data Case b = Case
  { _nextAct :: Action b,
    _gameState :: GameState b
  }

-- Convert a History into a list of Case, where a Case is generated:
--
getCases ::
  Bov.History (Amount "USD") ->
  Either (GameErrorWithCtx (Amount "USD")) [Case (Amount "USD")]
getCases hand' =
  let firstActCase = Case (head postflopActs) preflopState
   in fmap (firstActCase :) $
        runExcept
          . fmap snd
          . runWriterT
          . void
          . ($ firstActCase)
          $ concatM (go <$> tail postflopActs)
  where
    preflopState :: GameState (Amount "USD") =
      either (error . ppShow) id . flip runGame initState $
        mapM_
          emulateAction
          postActs
    initState = normalise hand'
    normalisedActs =
      mapMaybe normalise $
        Bov._handActions hand'
    (postActs, postflopActs) =
      let (preflopActs', postflopActs') =
            break
              (is (_MkDealerAction . only PlayerDeal))
              normalisedActs
       in ( preflopActs' `snoc` head postflopActs',
            filter (isn't _MkPostAction) $ tail postflopActs'
          )
    go ::
      ( IsBet b,
        Pretty b,
        MonadWriter [Case b] m,
        MonadError (GameErrorWithCtx b) m
      ) =>
      Action b ->
      Case b ->
      m (Case b)
    -- TODO needs some fenceposting
    go ac (Case nextA gs) = do
      gs' <- liftEither $ runActWithCtx gs nextA
      tell [Case  ac gs']
      pure $ Case  ac gs'

-- TODO choose some number in the middle too
getGoodTestActs :: IsBet b => AvailableAction b -> [BetAction b]
getGoodTestActs (ACall b) = [Call b]
-- TODO fix raiseBy
getGoodTestActs (ARaiseBetween b b') = [Raise mempty b, Raise mempty b']
-- TODO fix raiseBy
getGoodTestActs (ARaiseAllIn b) = [AllInRaise mempty b]
getGoodTestActs (AAllIn b) = [AllIn b]
getGoodTestActs AFold = [Fold]
getGoodTestActs ACheck = [Check]
getGoodTestActs (ABet b b') = [Bet b, Bet b']

-- TODO choose some number in the middle too
getBadTestActs :: (HasCallStack, IsBet b) => AvailableAction b -> [BetAction b]
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
  Int ->
  Either (GameErrorWithCtx (Amount "USD")) [Case b] ->
  IO ()
outputHand _hand originalFile handId casesOrErr = do
  case casesOrErr of
    Left geb -> do
      print $ "Found error while evaluating hand #" <> show handId <> " in " <> originalFile
      pPrint geb
    Right cases ->
      forM_
        cases
        ( \(Case nextA gs) -> do
            let availableActionsRes = availableActions gs
            case (nextA ^? _MkPlayerAction, availableActionsRes) of
              (Just pa@(PlayerAction playerPos ba), Right (pos', as)) -> do
                unless (playerPos == pos' && any (actionMatches ba) as) $
                  error $
                    "Found error while evaluating hand #" <> show handId <> " in " <> originalFile <> "\n"
                      <> ppShow (gs, pa)
                let goodTestActs = getGoodTestActs =<< as
                -- Test that all good acts are emulated without error
                forM_ goodTestActs $ \goodTestAct -> do
                  let res = runPlayerActWithCtx gs (PlayerAction playerPos goodTestAct)
                  case res of
                    Left geb -> do
                      print "Available actions are wrong"
                      pPrint geb
                    Right _ -> pure ()
                let badTestActs = getBadTestActs =<< as
                -- Test that all bad acts raise an error when emulated
                forM_ badTestActs $ \badTestAct ->
                  case runPlayerActWithCtx gs (PlayerAction playerPos badTestAct) of
                    Left _ -> pure ()
                    Right _ -> do
                      print "An action that should fail succeeded:"
                      putStrLn $
                        unlines
                          [ prettyString gs,
                            show playerPos,
                            show $ prettyString <$> badTestAct
                          ]
              (Just _, Left (T.unpack -> "No actors left")) -> pure ()
              (Just _, Left err) -> error (show err)
              -- TODO available actions for a finished game results in a Prelude.head exception
              _ -> pure ()
        )

data GameErrorWithCtx b = GameErrorWithCtx {state :: GameState b, offendingAct :: Action b, err :: GameError b}
  deriving (Show)

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM = foldr (>=>) pure

runGame :: StateT s (Except e) a -> s -> Either e s
runGame m = runExcept . execStateT m

runActWithCtx :: (IsBet b, Pretty b) => GameState b -> Action b -> Either (GameErrorWithCtx b) (GameState b)
runActWithCtx gs act =
  mapLeft (GameErrorWithCtx gs act) $
    runGame
      (emulateAction act)
      gs

runPlayerActWithCtx :: (IsBet b, Pretty b) => GameState b -> PlayerAction b -> Either (GameErrorWithCtx b) (GameState b)
runPlayerActWithCtx gs (MkPlayerAction -> act) = runActWithCtx gs act
