module Poker.Game.Bovada where
import qualified Poker.History.Bovada.Model as Bov
import Data.Maybe (mapMaybe)
import Poker.Game.Normalise
import Poker.Game.Types
import Poker.Game.Utils (runGame)
import Control.Lens.Extras
import Control.Lens
import Poker.Game.Emulate
import Prettyprinter
import Poker


preflopState :: (IsBet b, Pretty b) => Bov.History b -> (GameState b, [Action b])
preflopState hand' = case runGame (mapM_ emulateAction postActs) (normalise hand') of
  Left ge -> error $ show ge
  Right gs -> (gs, nonPostActs)
  where
    normalisedActs =
      mapMaybe normalise $
        Bov._handActions hand'
    (postActs, nonPostActs) =
      let (postActs', nonPostActs') =
            break
              (is (_MkDealerAction . only PlayerDeal))
              normalisedActs
        in ( postActs' `snoc` head nonPostActs',
            filter (isn't _MkPostAction) $ tail nonPostActs'
          )