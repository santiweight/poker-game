{-# LANGUAGE TupleSections #-}

module Test.Poker.Game.PokerStars.Emulate where

import Control.Lens hiding (Fold)
import Control.Lens.Extras (is)
import Control.Monad.Except
  ( Except,
    liftEither,
    runExcept,
  )
import Control.Monad.State.Strict
import Control.Monad.Writer
import qualified Data.ByteString as BS
import Data.Either.Extra (mapLeft)
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromJust,
    mapMaybe,
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Poker
import Poker.Game.AvailableActions
  ( AvailableAction (..),
    actionMatches,
    availableActions,
  )
import Poker.Game.Emulate
import Poker.Game.Normalise (Normalise (normalise))
import Poker.Game.Types
import qualified Poker.History.PokerStars.Model as PS
import qualified Poker.History.PokerStars.Parser as PS
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
  casesDir <- (</> "test/example-handhistories/2021-07-30_CO_NL50_FR_OOFGJJQ17") <$> getCurrentDirectory
  listDirectory casesDir <&> fmap (casesDir </>)

parseFile ::
  FilePath ->
  IO
    ( Either
        (ParseErrorBundle Text Void)
        [PS.History SomeBetSize]
    )
parseFile f = do
  file <- T.readFile f
  return . parse PS.pHands [] $ file

parseString ::
  Text ->
  Either (ParseErrorBundle Text Void) [PS.History SomeBetSize]
parseString = parse PS.pHands []

dropBOM :: BS.ByteString -> BS.ByteString
dropBOM bs
  | BS.take 3 bs == BS.pack [0xEF, 0xBB, 0xBF] = BS.drop 3 bs
  | otherwise = bs

parseHands :: FilePath -> IO [PS.History SomeBetSize]
parseHands fp = do
  handFileContents <- BS.readFile fp
  let handFileContents' = dropBOM handFileContents
  let res = parseString $ T.decodeUtf8With T.lenientDecode handFileContents'
  pure $ either (error . errorBundlePretty) id res

allHands :: IO (Map FilePath [PS.History (Amount "USD")])
allHands = do
  fps <- allHandFiles
  results <- (\fp -> (fp,) <$> parseHands fp) `mapM` fps
  let resultsUsd = (fmap . fmap . fmap) unsafeToUsdHand <$> results
  pure . Map.fromList $ resultsUsd

unit_testAllPokerStarsHands :: IO ()
unit_testAllPokerStarsHands = do
  fileResults <- Map.toList <$> Test.Poker.Game.PokerStars.Emulate.allHands
  cases <- execWriterT $
    forM fileResults $ \(fp, hands) ->
      forM hands $ \hand -> case getCases hand of
        Left err' -> do
          liftIO $ print $ "Skipping potentially corrupted history #" <> show (PS.gameId . PS.header $ hand) <> " in file " <> fp
          liftIO $ pPrint $ PS._handActions hand
          liftIO $ pPrint $ normalise <$> PS._handActions hand
          liftIO $ pPrint err'
        Right cas -> tell ((fp,hand,) <$> cas) >> pure ()
  print $ "Testing " <> show (sum $ length . snd <$> fileResults) <> " hands"
  print $ "Testing " <> show (length cases) <> " cases"
  forM_ cases $ \(fp, history, historyCase) -> do
    runCase
      fp
      (PS.gameId . PS.header $ history)
      historyCase

data Case b = Case
  { _nextAct :: Action b,
    _gameState :: GameState b
  }

-- Convert a History into a list of Case, where a Case is generated:
-- Total cases: 186572
--
getCases ::
  PS.History (Amount "USD") ->
  Either (GameErrorWithCtx (Amount "USD")) [Case (Amount "USD")]
getCases hand' = runExcept $ execWriterT $ foldlM go preflopState nonPostActs
  where
    go gs' act = do
      tell [Case act gs']
      liftEither $ runActWithCtx gs' act
    initState = normalise hand'
    preflopState = case runGame (mapM_ emulateAction postActs) initState of
      Left ge -> error $ ppShow ge
      Right gs -> gs
    normalisedActs =
      mapMaybe normalise $
        PS._handActions hand'
    (postActs, nonPostActs) =
      let (postActs', nonPostActs') =
            break
              (is (_MkDealerAction . only PlayerDeal))
              normalisedActs
       in ( postActs' `snoc` head nonPostActs',
            filter (isn't _MkPostAction) $ tail nonPostActs'
          )

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

runCase ::
  (Show b, Ord b, IsBet b, Pretty b) =>
  FilePath ->
  Int ->
  Case b ->
  IO ()
runCase originalFile handId (Case nextA gs) = do
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
    (Just _, Left err') -> error (show err')
    -- TODO available actions for a finished game results in a Prelude.head exception
    _ -> pure ()

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
