{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Trees where
-- Next big task - make it balanced (standard data structure) and use Okasaki suggestions on memory efficience. Finally I want to have effective data structure.
-- Also - make it key-value storage.
-- Add euristics. And more helpers.
-- Overload using '@IsList@'. Build Hashtree?

import Data.Maybe (fromMaybe)
import Data.Monoid

data Tree a = Leaf | Node (Tree a) a (Tree a)

class (
  Ord a,
--, Foldable t -- folds and toList (will produce sorted list)
--, Traversable t -- traverse
--, Functor t -- fmap = traverse
  Monoid t -- (<>) - join
  ) => BST a t where

  member :: a -> t -> Bool

  insert :: a -> t -> t

  -- find
  subTree :: a -> t -> Maybe t

  join :: t -> t -> t
  join = mappend

  -- to think
  -- merge - join with interferring sets.

  -- leave so but think of '@Divisible@' here.
  split :: a -> t -> (t, t)

  remove :: a -> t -> t

  -- remove item and all it's children.
  removeSub :: a -> t -> t

  fromList :: [a] -> t

-- Elements build set. Look at insert definition
instance  Ord a => BST a (Tree a) where

  member _ Leaf = False
  member item (Node left a right)
    | item < a = member item left
    | item > a = member item right
    | otherwise = True
  
  -- FIXME: rewrite using exceptions.
  -- FIXME: rebalance sometimes - there can be really bad roots.
  insert item Leaf = Node Leaf item Leaf
  insert item (Node left a right)
    | item < a = Node (insert item left) a right
    | item > a = Node left a (insert item right)
    | otherwise = Node left a right

  -- another implementation for '@find@' actually.
  subTree _ Leaf = Nothing
  subTree root (Node left a right)
    | root < a = subTree root left
    | root > a = subTree root right
    | otherwise = Just $ Node left a right

  removeSub _ Leaf = Leaf
  removeSub root (Node left a right)
    | root < a = Node (removeSub root left) a right
    | root > a = Node left a (removeSub root right)
    | otherwise = Leaf

  split _ Leaf = (Leaf, Leaf)
  split item tree = (fromMaybe Leaf (subTree item tree), removeSub item tree)

  remove _ Leaf = Leaf
  remove item (Node left a right)
    | item < a = Node (remove item left) a right
    | item > a = Node left a (remove item right)
    | otherwise = mappend left right

  -- naive
  fromList [] = Leaf
  fromList (x : xs) = insert x (fromList xs)

-- FIXME: implement.
-- I'll leave '@traverse@' and '@toList@' here.
-- instance Foldable (Tree a) where
-- instance Traversable (Tree a) where
-- instance Functor (Tree a) where

instance Ord a => Monoid (Tree a) where

  mempty = Leaf

  mappend Leaf Leaf = Leaf
  mappend Leaf tree = tree
  mappend tree Leaf = tree
  mappend (Node left a right) (Node left1 a1 right1)
    | a < a1 = Node (mappend (Node left a right) left) a1 right1
    | a1 <= a = Node (mappend (Node left1 a1 right1) left1) a right
    
