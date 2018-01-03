-- CONSTANTS

p = 10
pminus1 = (p-1)
pminus1s = pminus1:pminus1s
ones = 1:ones

-- ADDITION

add' :: [Integer] -> [Integer] -> Integer -> [Integer]
add' [] [] 0 = []
add' [] [] n = (n `rem` p):(add' [] [] (n `quot` p))
add' xs [] 0 = xs
add' (x:xs) [] n = if((x+n) < p) then (x+n):(add' xs [] 0) else ((x+n) `rem` p):(add' xs [] ((x+n) `quot` p))
add' [] ys 0 = ys
add' [] (y:ys) n = if((y+n) < p) then (y+n):(add' [] ys 0) else ((y+n) `rem` p):(add' [] ys ((y+n) `quot` p))
add' (x:xs) (y:ys) n = if((x+y+n) < p) then (x+y+n):(add' xs ys 0) else ((x+y+n) `rem` p):(add' xs ys ((x+y+n) `quot` p))

add :: [Integer] -> [Integer] -> [Integer]
add xs ys = add' xs ys 0

-- SUBTRACTION

sub :: [Integer] -> [Integer] -> [Integer]
sub [] [] = []
sub [] ys = sub (0:[]) ys
sub xs [] = xs 
sub (x:xs) (y:ys) = if(x >= y) then (x-y):(sub xs ys) else (p+x-y):(sub (borrow xs) ys)

borrow :: [Integer] -> [Integer]
borrow [] = pminus1s
borrow (x:xs) = if(x>0)then(x-1):xs else (p-1):(borrow xs)

-- MULTIPLICATION

addLoop :: Integer -> [Integer] -> [Integer]
addLoop 0 _  = []
addLoop n xs = add xs (addLoop (n-1) xs)

addShift :: [Integer] -> [Integer] -> [Integer]
addShift [] _ = []
addShift (x:xs) ys = x:(add xs ys)

mult :: [Integer] -> [Integer] -> [Integer]
mult [] _ = []
mult (x:xs) ys = addShift (addLoop x ys) (mult xs ys)

-- DIVISION



