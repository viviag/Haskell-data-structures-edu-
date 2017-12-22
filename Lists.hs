module Lists where
-- Naively on data.
-- Try to do it with type class - not to write contexts in signatures for everything.

-- Okasaki first exercise.
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs

-- values on leaves (only liquid garanties of being searching tree on start)
-- data Tree1 a = Leaf a | Node1 (Tree1 a) (Tree1 a) deriving (Eq, Show)

--  As far as i'm trying to write insert on this tree I see it cannot be.
-- insert :: a -> Tree a -> Tree a
-- insert item (Leaf a)
--   | item > a = Node (Leaf a) (Leaf item)
--   | otherwise = Node (Leaf item) (Leaf a)
-- insert item (Node left right)

data Tree a = End | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

insert :: Ord a => a -> Tree a -> Tree a
insert item End = Node End item End
insert item (Node left a right)
  | item < a = Node (insert item left) a right
  | otherwise = Node left a (insert item right)

-- Pushing error forward - then try backward - maybe I can do it without really strange context (Member a (Tree a)).
find :: Ord a => a -> Tree a -> Maybe (Tree a)
find item End = Nothing
find item (Node left a right)
  | item > a = find item right
  | item < a = find item left
  | otherwise = Just (Node left a right)

-- member1 :: Eq a => a -> Tree1 a -> Bool
-- member1 item (Leaf leaf) = item == leaf
-- member1 item (Node1 left right) = member item left || member item right

-- Not assuming '@Tree@' to be searching tree
member :: Eq a => a -> Tree a -> Bool
member item End = False
member item (Node left node right) = (item == node) || member item left || member item right

-- Assuming '@Tree@' to be searching
member1 :: Ord a => a -> Tree a -> Bool
member1 item End = False
member1 item (Node left node right)
  | item < node = member1 item left
  | item > node = member1 item right
  | otherwise = True
