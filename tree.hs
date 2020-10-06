
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)

-- pre order
instance Show a => Show (Tree a) where
  show Empty = ""
  show (Leaf v) = show v
  show (Node v l r) = foldr (++) "" [show v, " ", show l, " ", show r]

instance Foldable Tree where
  foldr _ i Empty = i
  foldr f i (Leaf v) = f v i
  foldr f i (Node v l r)
    = foldr f (foldr f (f v i) l) r

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Leaf x
insert x (Leaf v)
  | x < v  = Node v (Leaf x) Empty
  | x > v  = Node v Empty (Leaf x)
  | otherwise = Leaf v
insert x (Node v l r)
  | x < v = Node v (insert x l) r
  | x > v = Node v l (insert x r)
  | otherwise = Node v l r


listTree :: Ord a => [a] -> Tree a
listTree = foldl (flip insert) Empty

delete :: Ord a => a -> Tree a -> Tree a
delete x = foldr (\k r -> if (k==x) then r else (insert k r)) Empty

invert :: Tree a -> Tree a
invert (Node v l r) = Node v (invert r) (invert l)
invert x = x

names = ["judy", "mary", "bill", "alice", "tom", "fred", "jane", "joe", "dave"]

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Leaf v) = [v]
preOrder (Node v l r) = v : (preOrder l ++ preOrder r)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Leaf v) = [v]
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Leaf v) = [v]
postOrder (Node v l r) = postOrder l ++ postOrder r ++ [v]
