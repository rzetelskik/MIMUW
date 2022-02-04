module Main where
import Test.QuickCheck

infixr 5 :>
data Stream a = a :> (Stream a)

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : (streamToList xs)

instance Show a => Show (Stream  a) where
  show s = show $ take 20 $ streamToList s

rep :: a -> Stream a
rep a = a :> rep a

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

nats :: Stream Integer
nats = from 0

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
  pure x = rep x
  f :> fs <*> x :> xs = f x :> (fs <*> xs) 

newtype Supply s a = S { runSupply :: Stream s -> (a, Stream s) }

get :: Supply s s
get = S $ \(x :> xs) -> (x, xs)

pureSupply :: a -> Supply s a
pureSupply a = S $ \xs -> (a, xs)

mapSupply :: (a->b) -> Supply s a -> Supply s b
mapSupply f (S g) = S h where
  h s = let (a, xs) = g s in (f a, xs)

mapSupply2 :: (a->b->c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S ga) (S gb) = S gc where
  gc s = (f a b, ys) where 
    (a, xs) = ga s
    (b, ys) = gb xs

bindSupply :: Supply s a -> (a->Supply s b) -> Supply s b
bindSupply (S fa) k = S fb where
  fb s = fc xs where
    (a, xs) = fa s
    S fc = k a

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  (>>=) = bindSupply

evalSupply :: Supply s a -> Stream s -> a
evalSupply p s = fst $ runSupply p s

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Eq, Show)

size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = size l + size r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch l r) = toList l ++ toList r

labelTree :: Tree a -> Tree Integer
labelTree t = evalSupply (go t) nats
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf _) = Leaf <$> get
    go (Branch l r) = do
      lv <- go l
      rv <- go r
      return (Branch lv rv)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree'
    where tree' 0 = Leaf <$> arbitrary
          tree' n | n>0 =
                    oneof [Leaf <$> arbitrary,
                           Branch <$> subtree <*> subtree]
                    where subtree = tree' (n `div` 2)
  shrink (Leaf _) = []
  shrink (Branch l r) = [l, r] ++ [Branch l' r' | (l', r') <- shrink (l, r)]


prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size (labelTree t) == size t

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == [0..n]
    where n = fromIntegral $ size t - 1

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = labelTree (labelTree t) == labelTree t

writeln = putStrLn

main = do
  writeln "prop_sizeLabelTree"
  quickCheck prop_sizeLabelTree
  writeln "prop_LabelTree"
  quickCheck prop_labelTree
  writeln "prop_LabelTreeIdempotent"
  quickCheck prop_labelTreeIdempotent
