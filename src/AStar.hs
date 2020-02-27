{-# LANGUAGE TemplateHaskell #-}

module AStar where

import Protolude

import Linear.V2 (V2(..), _x, _y)
import Control.Lens (Lens', _Wrapped', Wrapped, (%=), (.=), (^.), use, (<&>), ifor_, (<>=))
import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.Set as Set

newtype FCost a = FCost { getFCost :: V2 a }
  deriving (Eq, Generic, Show)

instance Wrapped (FCost a)

instance (Num a, Ord a) => Ord (FCost a) where
  compare (FCost a) (FCost b) = compare (sum a) (sum b)

gcost :: Lens' (FCost a) a
gcost = _Wrapped' . _x

hcost :: Lens' (FCost a) a
hcost = _Wrapped' . _y

mkFCost :: a -> a -> FCost a
mkFCost g h = FCost (V2 g h)

data AStar a c = AStar
  { _openSet :: OrdPSQ a (FCost c) a
  , _asEdges :: Set (V2 a)
  , _path    :: Map a a
  , _scores  :: Map a (FCost c)
  }

makeLenses ''AStar

initAStar :: Num c => a -> c -> AStar a c
initAStar a ac =
  let cost = mkFCost 0 ac
  in AStar (PSQ.singleton a cost a) Set.empty Map.empty (Map.singleton a cost)

reconstruct :: Ord a => a -> AStar a c -> [a]
reconstruct l as = go [l] (as ^. path)
  where
    go xs m = case head xs >>= flip Map.lookup m of
      Nothing -> xs
      Just x  -> go (x : xs) m

astarM :: (Ord a, Ord c, Num c, Monad m)
       => a
       -> c
       -> (a -> AStar a c -> m Bool)
       -> (a -> AStar a c -> m c)
       -> (a -> AStar a c -> m (Map a c))
       -> m (Maybe (a, FCost c, AStar a c))
astarM start startCost isGoal heuristic adjacent = evalStateT go $ initAStar start startCost
  where
    go = do
      mcc <- dequeue
      case mcc of
        Just (curr, cost) -> do
          ig <- get >>= lift . isGoal curr
          if ig
            then Just . (curr, cost,) <$> get
            else handleAdj curr cost *> go
        Nothing -> pure Nothing

    handleAdj curr cost = get >>= lift . adjacent curr >>= \as -> ifor_ as \n nc -> do
      asEdges %= Set.insert (V2 curr n)
      let tgs = cost ^. gcost + nc
      mns <- use scores <&> Map.lookup n
      case mns of
        Just ns | tgs < ns ^. gcost -> newPath curr n tgs
                | otherwise         -> pure ()
        Nothing -> newPath curr n tgs

      where
        newPath c n tgs = do
          hs <- get >>= lift . heuristic n
          let score = mkFCost tgs hs
          path    %= Map.insert n c
          scores  %= Map.insert n score
          openSet %= PSQ.insert n score n

    dequeue = do
      mmv <- use openSet <&> PSQ.minView
      case mmv of
        Just (_, p, v, q') -> do
          openSet .= q'
          pure $ Just (v, p)
        Nothing -> pure Nothing

data Graph' f i a = Graph
  { _nodes :: f a
  , _edges :: f (V2 i)
  }

type Graph i a = Graph' Set i a

makeLenses ''Graph'

hoistGraph :: (forall x. f x -> g x) -> Graph' f i a -> Graph' g i a
hoistGraph nat (Graph ns es) = Graph (nat ns) (nat es)

initGraph :: a -> Graph i a
initGraph a = Graph (Set.singleton a) Set.empty

floodFillM :: (Ord i, Ord a, Monad m) => a -> (a -> i) -> (i -> m (Set a)) -> Int -> m (Graph i a)
floodFillM start proj neighbors depth = execStateT (go 0 $ Set.singleton start) $ initGraph start
  where
    go n s
      | n >= depth || Set.null s = pure ()
      | otherwise = do
          xs <- getAp $ foldMap explore s
          nodes <>= xs
          go (n + 1) xs

    explore a = Ap do
      ns <- lift $ neighbors $ proj a
      traverse_ (\n -> edges %= Set.insert (V2 (proj a) (proj n))) ns
      use nodes <&> Set.difference ns
