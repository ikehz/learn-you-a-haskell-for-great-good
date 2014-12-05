data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

changeW :: Tree Char -> Tree Char
changeW (Node p o (Node l (Node _ c r) a)) = Node p o (Node l (Node 'P' c r) a)

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Take a tree along with a list of directions.  The directions will be either L or R, representing
-- left and right respectively, and we'll change the element that we arrive at if we follow the
-- supplied directions.
changeToP :: Directions -> Tree Char -> Tree Char
changeToP [] (Node _ l r) = Node 'P' l r
changeToP (L:ds) (Node c l r) = Node c (changeToP ds l) r
changeToP (R:ds) (Node c l r) = Node c l (changeToP ds r)

-- Take a list of directions and tells us what the element at the destination is.
elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

-- Take a tree and some breadcrumbs and move to the left sub-tree while adding L to the head of the
-- list that represents our breadcrumbs
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, (LeftCrumb x r):bs)
goLeft (Empty, _) = Nothing

-- And right
goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, (RightCrumb x l):bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (l, (LeftCrumb x r):bs) = Just (Node x l r, bs)
goUp (r, (RightCrumb x l):bs) = Just (Node x l r, bs)
goUp (_, []) = Nothing

-- Modify the element in the root of the sub-tree that the zipper is focusing on
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, bs) = case (goUp (t, bs)) of
    Just (r, cs) -> topMost (r, cs)
    Nothing -> (t, bs)
