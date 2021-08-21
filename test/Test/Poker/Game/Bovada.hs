{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Test.Poker.Game.Bovada where

import           Control.Lens                   ( (%%=)
                                                , zoom
                                                )
import           Control.Lens.Combinators
import           Control.Lens.Extras            ( is )
import           Control.Monad.Error
import           Control.Monad.Except           ( Except
                                                , liftEither
                                                , runExcept
                                                , runExceptT
                                                )
import           Control.Monad.Identity         ( Identity(runIdentity)
                                                , IdentityT(runIdentityT)
                                                )
import           Control.Monad.State.Strict     ( MonadState(get, put)
                                                , StateT(runStateT)
                                                , execStateT
                                                , runState
                                                )
import           Control.Monad.Writer
import           Data.Foldable                  ( foldl' )
import           Data.Functor
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Void                      ( Void )
import           Extra                   hiding ( snoc )
import           GHC.Num                        ( integerToInt )
import           Money                          ( Approximation(Floor)
                                                , defaultDecimalConf
                                                , dense'
                                                , discreteFromDense
                                                , discreteToDecimal
                                                )
import           Poker.Base
import           Poker.Game.AvailableActions    ( availableActions )
import           Poker.Game.Bovada
import           Poker.Game.Types
import qualified Poker.History.Parse.Bovada    as Bov
import qualified Poker.History.Types           as Bov
import           Poker.History.Types            ( History(_handActions) )
import           Prettyprinter                  ( LayoutOptions(layoutPageWidth)
                                                , Pretty(pretty)
                                                , defaultLayoutOptions
                                                , layoutCompact
                                                , layoutPretty
                                                , line
                                                , viaShow
                                                , vsep
                                                )
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           Test.Tasty.HUnit
import           Text.Megaparsec
import           Text.Regex.Base
import           Text.Regex.Posix
import           Text.Show.Pretty               ( ppShow
                                                , ppShowList
                                                )

testDir :: IO FilePath
testDir = getCurrentDirectory <&> (</> "test")

allHandFiles :: IO [FilePath]
allHandFiles = do
  casesDir <- (</> "test/example-handhistories/Bovada") <$> getCurrentDirectory
  listDirectory casesDir <&> fmap (casesDir </>)

parseFile
  :: FilePath
  -> IO
       ( Either
           (ParseErrorBundle Text Void)
           [Bov.History Bov.Bovada Bov.SomeBetSize]
       )
parseFile f = do
  file <- T.readFile f
  return . parse Bov.pHands [] $ file

allHands :: IO [Bov.History Bov.Bovada Bov.SomeBetSize]
allHands = do
  fps     <- allHandFiles
  results <- parseFile `mapM` fps
  pure
    . concatMap (fromRight (error "should all parse without error"))
    $ results

makePrisms ''Action

outputAvailableActions :: IO ()
outputAvailableActions = do
  outputDir    <- testDir <&> (</> "output")
  doesDirExist <- doesDirectoryExist outputDir
  when doesDirExist $ removeDirectoryRecursive outputDir
  createDirectory outputDir
  hands <- take 10 <$> allHands
  forM_ (zip [1..] hands) $ \(i, hand) -> do
    print "hi"
    let handOutputDir = outputDir </> show i
    createDirectory handOutputDir
    let hand' = fmap
          (\(Bov.SomeBetSize Bov.USD ra) ->
            Amount . fst . discreteFromDense Floor $ dense' ra
          )
          hand
    outputHand handOutputDir hand'

outputHand :: FilePath -> Bov.History Bov.Bovada (Amount "USD") -> IO ()
outputHand handOutputDir hand' = do
  let (preflop, postflop) =
        break (is (_MkDealerAction . only PlayerDeal)) $ _handActions hand'
  let (preflop', postflop') =
        ( preflop <> [head postflop]
        , filter (isn't _MkTableAction) $ tail postflop
        )
  let initState = bovadaHistoryToGameState hand'
  let st :: GameState (Amount "USD") =
        either (error . ppShow) id . flip runGame initState $ mapM_
          emulateAction
          preflop'
  -- let outFile = outputDir </> "out"
  case
      runExcept . fmap snd . runWriterT . void . flip execStateT ([], st) $ mapM
        go
        postflop'
    of
      Left  geb -> writeFile (handOutputDir </> "bad") $ prettyString geb
      Right x0  -> forM_
        (zip [1 ..] (([], st) : x0))
        (\arg@(ix, (acs, gs)) -> do
          let caseOutputFile = handOutputDir </> show ix
          writeFile caseOutputFile . unlines . fmap (show . fmap pretty) $ acs
          appendFile caseOutputFile "\n\n"
          appendFile caseOutputFile
            . renderString
            . layoutPretty defaultLayoutOptions
            . (\doc -> doc <> line <> line)
            . pretty
            $ gs
          appendFile caseOutputFile
            . renderString
            . layoutPretty defaultLayoutOptions
            . either pretty pretty
            . availableActions
            $ gs
        )

  -- writeFile outFile $ show prevAs
  -- appendFile outFile
  --   . renderString
  --   . layoutPretty defaultLayoutOptions
  --   . (\doc -> doc <> line <> line)
  --   . pretty
  --   $ st
  -- appendFile outFile
  --   . renderString
  --   . layoutPretty defaultLayoutOptions
  --   . either pretty pretty
  --   . availableActions
  --   $ st
  -- pure ()
 where
  prettyString :: Pretty a => a -> String
  prettyString = renderString . layoutPretty defaultLayoutOptions . pretty
  go
    :: ( Num b
       , Ord b
       , MonadWriter [([Action b], GameState b)] m
       , MonadError (GameErrorBundle b) m
       , MonadState ([Action b], GameState b) m
       )
    => Action b
    -> m ()
  go ac = do
    (as, gs) <- get
    gs'      <- liftEither $ flip runGame gs $ emulateAction ac
    put (as `snoc` ac, gs')
    tell [(as `snoc` ac, gs')]

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM = foldr (>=>) pure


runGame :: StateT s (Except e) a -> s -> Either e s
runGame m = runExcept . execStateT m

instance Pretty (Amount b) where
  pretty (Amount dis) = pretty $ discreteToDecimal defaultDecimalConf Floor dis

unit_output :: IO ()
unit_output = outputAvailableActions

bovadaHistoryToGameState :: Num b => Bov.History Bov.Bovada b -> GameState b
bovadaHistoryToGameState Bov.History { Bov._handStakes, Bov._handPlayerMap, Bov._handSeatMap, Bov._handActions, Bov._handText }
  = GameState
    { _potSize           = PotSize 0
    , _street            = InitialTable
    , _stateStakes       = _handStakes
    , _aggressor         = Nothing
    , _toActQueue        = Map.keys _wE
              -- , _pastActions       = []
              -- , _futureActions     = _handActions
    , _posToPlayer       = normalizePlayer <$> _wE
    , _streetInvestments = Map.empty
    , _activeBet = Just ActionFaced { _betType     = PostB
                                    , _amountFaced = getStake _handStakes
                                    , _raiseSize   = getStake _handStakes
                                    }
    }
 where
      -- TODO remove unsafe fromJust
      -- Should the hands dealt to players be declared after the initialtable?
      -- Or should GameState be a data family so we can pattern match on the current street
  normalizePlayer :: Bov.Player b -> Player b
  normalizePlayer (Bov.Player m_ha b) =
    Player { _playerHolding = fromJust m_ha, _stack = Stack b }
  _wE = Map.mapMaybe (`Map.lookup` _handPlayerMap) _handSeatMap--
