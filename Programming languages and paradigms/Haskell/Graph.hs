module Graph where
import Set(Set)
import qualified Set as Set
import qualified Data.List

class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty         = Relation {domain = mempty, relation = mempty}
  vertex a      = Relation {domain = Set.singleton a, relation = mempty}
  union g1 g2   = Relation {domain = (domain g1) <> (domain g2), relation = (relation g1) <> (relation g2)}
  connect g1 g2 = let u = union g1 g2 in u {relation = relation u <> Set.fromList [(x,y) | x <- Set.toList (domain g1), y <- Set.toList (domain g2)]}
    
instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

instance Graph Basic where
  empty   = Empty

  vertex  = Vertex
  
  union Empty v = v
  union u Empty = u
  union u v = Union u v

  connect Empty v = v
  connect u Empty = u
  connect u v = Connect u v

instance Ord a => Eq (Basic a) where
  u == v = (fromBasic::Basic a -> Relation a) u == (fromBasic::Basic a -> Relation a) v

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union u v) = union (fromBasic u) (fromBasic v)
fromBasic (Connect u v) = connect (fromBasic u) (fromBasic v)

describeRelation :: Ord a => Relation a -> ([(a,a)], [a])
describeRelation g = (edges,vertices) where
  vertices = difference (Set.toAscList (domain g)) (Data.List.sort (foldr (\(a,b) es -> a:b:es) [] edges))
  edges = Set.toAscList $ relation g

  difference :: Ord a => [a] -> [a] -> [a]
  difference [] ys = []
  difference xs [] = xs
  difference (x:xs) (y:ys) | x < y     = x : difference xs (y:ys)
                           | x == y    = difference xs (y:ys)
                           | otherwise = difference (x:xs) ys

instance (Ord a, Show a) => Show (Basic a) where
  show g = showsPrec 0 g ""

  showsPrec _ g = showString "edges " . showList edges . showString " + vertices " . showList vertices where
    (edges, vertices) = describeRelation $ fromBasic g

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = todot' g "" where
  todot' :: (Ord a, Show a) => Basic a -> ShowS
  todot' g = showString "digraph {\n" . edgesTodot edges . verticesTodot vertices . showString "}\n"

  (edges, vertices) = describeRelation (fromBasic g)

  edgesTodot :: (Ord a, Show a) => [(a,a)] -> ShowS
  edgesTodot [] s = s
  edgesTodot ((x,y):xs) s = shows x . showString " -> " . shows y . showString ";\n" . edgesTodot xs $ s 

  verticesTodot :: (Ord a, Show a) => [a] -> ShowS
  verticesTodot [] s = s
  verticesTodot (x:xs) s = shows x . showString ";\n" . verticesTodot xs $ s

  shows :: (Ord a, Show a) => a -> ShowS
  shows = showsPrec 0

instance Functor Basic where
  fmap _ Empty = empty
  fmap f (Vertex a) = vertex (f a)
  fmap f (Union u v) = union (fmap f u) (fmap f v)
  fmap f (Connect u v) = connect (fmap f u) (fmap f v)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c = fmap (\x -> if x==a || x==b then c else x)

instance Applicative Basic where
  pure = vertex
  Empty <*> _ = empty
  (Vertex f) <*> u = f <$> u
  (Union f g) <*> u = union (f <*> u) (g <*> u)
  (Connect f g) <*> u = connect (f <*> u) (g <*> u)

instance Monad Basic where
  Empty >>= f = empty
  (Vertex a) >>= f = f a
  (Union u v) >>= f = union (u >>= f) (v >>= f)
  (Connect u v) >>= f = connect (u >>= f) (v >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = g >>= (\x -> if x == a then vertex b <> vertex c else vertex x)
