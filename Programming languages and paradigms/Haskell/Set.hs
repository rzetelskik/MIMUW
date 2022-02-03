module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import qualified Data.List

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member e (Singleton a) = e == a
member e (Union s1 s2) = member e s1 || member e s2
member _ _ = False

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldr insert empty 

toList :: Set a -> [a]
toList s = prepend s [] where
    prepend :: Set a -> [a] -> [a]
    prepend Empty es = es
    prepend (Singleton a) es = a:es
    prepend (Union s1 s2) es = prepend s1 (prepend s2 es)

toAscList :: Ord a => Set a -> [a]
toAscList = map head . Data.List.group . Data.List.sort . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union Empty s2 = s2
union s1 Empty = s1
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert e = union (singleton e)

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    (<>) = union

instance Monoid (Set a) where
    mempty = empty

instance Show a => Show (Set a) where
    show = show . toList

instance Functor Set where
    fmap f Empty = empty
    fmap f (Singleton a) = singleton (f a)
    fmap f (Union s1 s2) = (fmap f s1) <> (fmap f s2)
