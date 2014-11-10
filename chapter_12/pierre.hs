type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (l, r)
    | abs ((l+b) - r) < 4 = Just (l+b, r)
    | otherwise           = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight b (l, r)
    | abs (l - (r+b)) < 4 = Just (l, r+b)
    | otherwise           = Nothing

x -: f = f x

routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second  
