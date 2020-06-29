
--TODO 2.6, 2.35

--Ex 2.1 -----------------------------------------------------------------------------------------------------------------

-- Raw data
data Pair = Pair Int Int deriving (Show, Eq)

-- API
numer :: Pair -> Int
numer (Pair x y) = x

denom :: Pair -> Int
denom (Pair x y) = y

makeRat :: Int -> Int -> Pair
makeRat n d = Pair numerator denominator

               where divisor = gcd n d
                     numerator = if ((n < 0) `xor` (d < 0)) then ((-1) * abs n) `div` divisor else (abs n) `div` divisor
                     denominator = (abs d) `div` divisor


--Ex 2.2 -----------------------------------------------------------------------------------------------------------------
-- Raw Data
data Point = Point Double Double deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show, Eq)

--API
makePoint :: Double -> Double -> Point
makePoint = Point 

xPoint :: Point -> Double
xPoint (Point x y) = x

yPoint :: Point -> Double
yPoint (Point x y) = y

makeSegment :: Point -> Point -> Segment
makeSegment x y = Segment x y

--Example of functions that use pattern matching. While the book uses selectors that provide a layer of abstraction over the underlying representation,
--I opted to use pattern matching in some cases because of how clean and concise it makes the code. Pattern matched code is really easy to reason about and find mistakes in.
--I
startSegment :: Segment -> Point
startSegment (Segment x y) = x

endSegment :: Segment -> Point
endSegment (Segment x y) = y

-- Example of a function that doesn't use pattern matching, much less concise
midpoint :: Segment -> Point
midpoint segment = makePoint xAverage yAverage
                   
                   where xAverage = (average (xPoint (startSegment segment)) (xPoint (endSegment segment)))
                         yAverage = (average (yPoint (startSegment segment)) (yPoint (endSegment segment)))


--Ex 2.3 -----------------------------------------------------------------------------------------------------------------
--Raw data
data Rectangle = Rectangle Point Point Point Point deriving (Eq, Show)
data RectangleAlt = RectangleAlt Point Double Double deriving (Eq, Show)

--First implementation
makeRectangle :: Point -> Point -> Point -> Point -> Rectangle
makeRectangle = Rectangle

topLeftRectangle :: Rectangle -> Point
topLeftRectangle (Rectangle x y a b) = x

topRightRectangle :: Rectangle -> Point
topRightRectangle (Rectangle x y a b) = y

bottomLeftRectangle :: Rectangle -> Point
bottomLeftRectangle (Rectangle x y a b) = a

bottomRightRectangle :: Rectangle -> Point
bottomRightRectangle (Rectangle x y a b) = b

xLength :: Rectangle -> Double
xLength(Rectangle (Point i o) (Point q w) a b)  = abs (i - q) + abs (o - w) 

yLength :: Rectangle -> Double
yLength (Rectangle (Point i o) y (Point q w) b) = abs (i - q) + abs (o - w) 

--Second implementation
makeRectangleAlt :: Point -> Double -> Double -> RectangleAlt
makeRectangleAlt = RectangleAlt

xLengthAlt :: RectangleAlt -> Double
xLengthAlt (RectangleAlt _ xSize _) = xSize

yLengthAlt :: RectangleAlt -> Double
yLengthAlt (RectangleAlt _ _ ySize) = ySize

-- General abstract functions that apply to both implementations
perimeter :: RectangleAlt -> Double
perimeter rect = (xLengthAlt rect) * 2 + (yLengthAlt rect) * 2

area :: RectangleAlt -> Double
area rect = (xLengthAlt rect) * (yLengthAlt rect)


--Ex 2.4 -----------------------------------------------------------------------------------------------------------------
type EncodedDataInFunction = ((Int -> Int -> Int) -> Int)

consSicp :: Int -> Int -> EncodedDataInFunction
consSicp  x y = (\m -> m x y)

carSicp  :: EncodedDataInFunction -> Int
carSicp  c = c (\p q -> p)

cdrSicp  :: EncodedDataInFunction -> Int
cdrSicp  c = c (\p q -> q)


--Ex 2.5 -----------------------------------------------------------------------------------------------------------------
consAlt :: Int -> Int -> Int
consAlt a b = 2 ^ a * 3 ^ b

carAlt :: Int -> Int
carAlt y = if (isRoot y 2) then (floorLog 2 y)            
            else carAlt (y `div` 3)

cdrAlt :: Int -> Int
cdrAlt y = if (isRoot y 3) then (floorLog 3 y)            
            else cdrAlt (y `div` 2)

isRoot x n = if (x == 1) then True 
              else if (x `mod` n /= 0) then False 
              else isRoot (x `div` n) n

--Ex 2.6 NOT DONE YET ----------------------------------------------------------------------------------------------------------------------------------------------------------
zero :: (a -> a) -> a -> a
zero = (\f -> (\x -> x))

one :: (a -> a) -> a -> a
one = (\f -> (\x -> f x))

two :: (a -> a) -> a -> a
two = (\f -> (\x -> f $ f x))

add :: (a -> a) -> a -> a -> (a -> a) -> a -> a
add f g = undefined


--Ex 2.7 -----------------------------------------------------------------------------------------------------------------
--Raw data
data Interval = Interval Double Double deriving (Eq, Show)

--API
makeInterval :: Double -> Double -> Interval
makeInterval x y = Interval (min x y) (max x y)

lowerBound :: Interval -> Double
lowerBound (Interval a b) = a

upperBound :: Interval -> Double
upperBound (Interval a b) = b

width :: Interval -> Double
width (Interval x y) = 0.5 * (y - x)


--Ex 2.8 -----------------------------------------------------------------------------------------------------------------
subInterval :: Interval -> Interval -> Interval
subInterval (Interval l1 u1) (Interval l2 u2) = makeInterval (l1 - u2) (u1 - l2)


--Ex 2.9 -----------------------------------------------------------------------------------------------------------------
mulInterval :: Interval -> Interval -> Interval
mulInterval (Interval l1 u1) (Interval l2 u2) = Interval (minimum combinations) (maximum combinations)

                                                 where combinations = [l1 * l2, l1 * u2, u1 * l2, u1 * u2]

divInterval :: Interval -> Interval -> Interval
divInterval x (Interval l2 u2) = mulInterval x (makeInterval (1 / l2) (1 / u2))

--Example for mult, arguments are of the same width but returns different values
mul1 = mulInterval (Interval 2.5 3.6) (Interval 3.2 4.8)
mul2 = mulInterval (Interval 1.2 2.3) (Interval 1.9 3.5)
isSameMul = width mul1 == width mul2

--Example for div, again arguments are of the same width but returns different values
div1 = divInterval (Interval 2.5 3.6) (Interval 3.2 4.8)
div2 = divInterval (Interval 1.2 2.3) (Interval 1.9 3.5)
isSameDiv = width div1 == width div2


--Ex 2.10 ----------------------------------------------------------------------------------------------------------------
divIntervalZeroCheck :: Interval -> Interval -> Interval
divIntervalZeroCheck x (Interval l2 u2) = if (l2 == 0 || u2 == 0) then error "Can't divide an interval that spans zero"
                                             else mulInterval x (makeInterval (1 / l2) (1 / u2))


--Ex 2.11 ----------------------------------------------------------------------------------------------------------------
mulIntervalCases :: Interval -> Interval -> Interval
mulIntervalCases (Interval l1 u1) (Interval l2 u2) | l1 < 0 && u1 < 0 && l2 < 0 && u2 < 0 = Interval (u1 * u2) (l1 * l2)
                                                   | l1 > 0 && u1 > 0 && l2 > 0 && u2 > 0 = Interval (l1 * l2) (u1 * u2) 
                                                   | l1 < 0 && u1 < 0 && l2 > 0 && u2 > 0 = Interval (l1 * u2) (u1 * l2)
                                                   | l1 > 0 && u1 > 0 && l2 < 0 && u2 < 0 = Interval (u1 * l2) (l1 * u2)
                                                   | l1 > 0 && u1 > 0 && l2 < 0 && u2 > 0 = Interval (u1 * l2) (u1 * u2)
                                                   | l1 < 0 && u1 > 0 && l2 > 0 && u2 > 0 = Interval (u2 * l1) (u2 * u1)
                                                   | l1 < 0 && u1 > 0 && l2 < 0 && u2 < 0 = Interval (u1 * l2) (l1 * l2)
                                                   | l1 < 0 && u1 < 0 && l2 < 0 && u2 > 0 = Interval (u2 * l1) (l2 * l1)             

                                                     -- Case where more than 2 multiplications are needed
                                                   | l1 < 0 && u1 > 0 && l2 < 0 && u2 > 0 = Interval (min (l1 * u2) (l2 * u1)) (max (l2 * u2) (l1 * l2))


--Ex 2.12 ----------------------------------------------------------------------------------------------------------------
makeCenterPercent :: Double -> Double -> Interval 
makeCenterPercent center tolerance = makeInterval lowerBound upperBound

                                       where toleranceAsDecimal = tolerance / 100
                                             lowerBound = center - (toleranceAsDecimal * center)
                                             upperBound = center + (toleranceAsDecimal * center)


--Ex 2.14 ----------------------------------------------------------------------------------------------------------------
addInterval :: Interval -> Interval -> Interval
addInterval (Interval l1 u1) (Interval l2 u2) = makeInterval (l1 + l2) (u1 + u2)

par1 :: Interval -> Interval -> Interval
par1 r1 r2 = (mulInterval r1 r2) `divInterval` (addInterval r1 r2)

par2 :: Interval -> Interval -> Interval
par2 r1 r2 = one `divInterval` ((one `divInterval` r1) `addInterval` (one `divInterval` r2))

             where one = makeInterval 1 1

isEqualTest1 :: Bool
isEqualTest1 = par1 (makeInterval 9.9 10) (makeInterval 11.7 12) == par2 (makeInterval 9.9 10) (makeInterval 11.7 12)

isEqualTest2 :: Bool
isEqualTest2 = par1 (makeInterval 1 2) (makeInterval 3 4) == par2 (makeInterval 1 2) (makeInterval 3 4)

isEqualTest3 :: Bool
isEqualTest3 = par1 (makeInterval 9.99 10) (makeInterval 11.78 12) == par2 (makeInterval 9.99 10) (makeInterval 11.78 12)


--Ex 2.17 ----------------------------------------------------------------------------------------------------------------
lastPair :: [a] -> a
lastPair xs = if (null xs) then error "No last element in an empty list"
               else if null $ cdr xs then car xs
               else lastPair $ cdr xs

--Ex 2.18 ----------------------------------------------------------------------------------------------------------------
reverseSicp :: [a] -> [a]
reverseSicp xs = if (null xs) then xs
                  else (reverseSicp $ cdr xs) ++ [car xs]


--Ex 2.19 ----------------------------------------------------------------------------------------------------------------
firstDenomination :: (Num a) => [a] -> a
firstDenomination = car 

exceptFirstDenomination :: (Num a) => [a] -> [a]
exceptFirstDenomination = cdr

noMore :: (Num a) => [a] -> Bool
noMore = null


--Ex 2.20 ----------------------------------------------------------------------------------------------------------------
sameParity :: [Int] -> [Int]
sameParity xs = filter (\n -> n `mod` 2 == (car xs) `mod` 2) xs


--Ex 2.21 ----------------------------------------------------------------------------------------------------------------
squareList :: (Num a) => [a] -> [a]
squareList xs = if (null xs) then []
                 else (square $ car xs):(squareList $ cdr xs)

squareListMap :: (Num a) => [a] -> [a]
squareListMap = map (^2)


--Ex 2.23 ----------------------------------------------------------------------------------------------------------------
forEach [] _ = do 
             return ()

forEach xs f = do
             f $ car xs
             forEach (cdr xs) f


--Ex 2.27 ----------------------------------------------------------------------------------------------------------------
-- The Tree data type is essentially a stand in for Lisps 'list of lists' idea, which doesn't have a built-in equivalent in Haskell
-- 'appendTree' stands in for Lisps 'append'. It's basically Haskell's ++ operator for the tree datatype

data Tree a = Leaf a | Node [Tree a] deriving (Read, Show, Eq)

instance Functor Tree where 
    fmap f (Leaf a) = Leaf $ f a
    fmap _ (Node []) = Node []
    fmap f (Node (x:xs)) = (fmap f x) `appendTree` (fmap f $ Node xs)

deepReverse :: Tree a -> Tree a
deepReverse (Leaf a) = Leaf a
deepReverse (Node []) = Node []
deepReverse (Node (x:[])) = Node (x:[])
deepReverse (Node (x:xs)) = (deepReverse $ Node xs) `appendTree` (deepReverse x)

appendTree :: Tree a -> Tree a -> Tree a
appendTree (Leaf x) (Leaf y) = Node ([Leaf x] ++ [Leaf y])
appendTree (Node xs) (Leaf y) = Node (xs ++ [Leaf y])
appendTree (Leaf x) (Node ys) = Node ((Leaf x):ys)
appendTree (Node xs) (Node ys) = Node (xs ++ ys)


--Ex 2.28 ----------------------------------------------------------------------------------------------------------------
fringe :: Tree a -> Tree a
fringe (Leaf a) = Leaf a
fringe (Node []) = Node []
fringe (Node (x:xs)) = x `appendTree` (fringe $ Node xs)


--Ex 2.29 ----------------------------------------------------------------------------------------------------------------
-- I decided to define the data structures the same way the book describes it. It ends up being quite awkward but it works
data BinaryMobile = BinaryMobile Branch Branch deriving (Eq, Read, Show)
data Branch = RecursiveMobile Int BinaryMobile | Weight Int Int deriving (Eq, Read, Show)

--Selectors
makeMobile :: Branch -> Branch -> BinaryMobile
makeMobile left right = BinaryMobile left right

leftBranch :: BinaryMobile -> Branch
leftBranch (BinaryMobile x y) = x

rightBranch :: BinaryMobile -> Branch
rightBranch (BinaryMobile x y) = y

branchLength :: Branch -> Int
branchLength (RecursiveMobile length _) = length
branchLength (Weight length _) = length

branchStructure :: Branch -> (Either Int BinaryMobile)
branchStructure (Weight _ val) = Left val
branchStructure (RecursiveMobile _ mobile) = Right mobile

--Compute functions
totalWeightMobile :: BinaryMobile -> Int
totalWeightMobile (BinaryMobile left right) = totalWeightBranch left + totalWeightBranch right

totalWeightBranch :: Branch -> Int
totalWeightBranch (RecursiveMobile _ mobile) = totalWeightMobile mobile
totalWeightBranch (Weight _ val) = val

isBalanced :: BinaryMobile -> Bool
isBalanced (BinaryMobile left right) = branchTorque left == branchTorque right && isBalancedBranch left && isBalancedBranch right

isBalancedBranch :: Branch -> Bool
isBalancedBranch (RecursiveMobile length mobile) = isBalanced mobile
isBalancedBranch (Weight _ _) = True

branchTorque :: Branch -> Int
branchTorque (RecursiveMobile length mobile) = length * (mobileTorque mobile)
branchTorque (Weight length value) = length * value

mobileTorque :: BinaryMobile -> Int
mobileTorque (BinaryMobile left right) = branchTorque left + branchTorque right


--Ex 2.30 ----------------------------------------------------------------------------------------------------------------
squareTree :: (Num a) => Tree a -> Tree a
squareTree (Leaf a) = Leaf $ square a
squareTree (Node []) = Node []
squareTree (Node (x:xs)) = (squareTree x) `appendTree` (squareTree $ Node xs)


--Ex 2.31 ----------------------------------------------------------------------------------------------------------------
treeMap :: Tree a -> (a -> b) -> Tree b
treeMap (Leaf a) f = Leaf $ f a
treeMap (Node []) f = Node []
treeMap (Node (x:xs)) f = (treeMap x f) `appendTree` (treeMap (Node xs) f)


--Ex 2.32 ----------------------------------------------------------------------------------------------------------------
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = foldl (\acc n -> (x:n):acc) (powerset xs) (powerset xs)


--Ex 2.33 ----------------------------------------------------------------------------------------------------------------
mapSicp :: (a -> b) -> [a] -> [b]
mapSicp f = foldr (\n acc -> (f n):acc) [] 

append :: [a] -> [a] -> [a]
append xs ys = foldr (\n acc -> n:acc) ys xs

lengthSicp :: [a] -> Int
lengthSicp = foldl (\acc n -> acc + 1) 0


--Ex 2.34 ----------------------------------------------------------------------------------------------------------------
hornerEval :: Int -> [Int] -> Int
hornerEval x = foldr (\n acc -> (acc * x) + n) 0 


--Ex 2.35 ----------------------------------------------------------------------------------------------------------------
countLeaves :: Tree a -> Int
countLeaves tree = undefined


--Ex 2.36 ----------------------------------------------------------------------------------------------------------------
accumulateN :: (a -> a -> a) -> a -> [[a]] -> [a]
accumulateN f startVal seq = if (null $ car seq) then []
                               else (foldl f startVal getFirstElems):(accumulateN f startVal getRest)

                               where getFirstElems = map car seq
                                     getRest = map cdr seq

--Ex 2.37 ----------------------------------------------------------------------------------------------------------------
type Vector = [Double]
type Matrix = [Vector]

matrixVectorProd :: Matrix -> Vector -> Vector
matrixVectorProd [] vector = []
matrixVectorProd (row:matrix) vector = (foldl (+) 0 terms):(matrixVectorProd matrix vector)

                                            where terms = zipWith (*) row vector 

matrixProd :: Matrix -> Matrix -> Matrix
matrixProd [] _ = []
matrixProd m n = (matrixProdRow m n):(matrixProd (cdr m) n)

dotProd :: Vector -> Vector -> Double
dotProd [] _ = 0
dotProd _ [] = 0
dotProd v1 v2 = foldl (+) 0 (zipWith (*) v1 v2) 

transpose :: Matrix -> Matrix
transpose [] = []
transpose ([]:m) = []
transpose m = (carColumn m):(transpose $ cdrColumn m)


--Ex 2.39 ----------------------------------------------------------------------------------------------------------------
reverseFoldr :: [a] -> [a]
reverseFoldr xs = foldr (\n acc -> acc ++ [n]) [] xs

reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl (\acc n -> n:acc) [] xs


--Helper functions
matrixProdRow :: Matrix -> Matrix -> Vector
matrixProdRow _ [] = []
matrixProdRow _ ([]:n) = []
matrixProdRow m n = (dotProd (car m) (carColumn n)):(matrixProdRow m (cdrColumn n))

carColumn :: Matrix -> Vector
carColumn = map car

cdrColumn :: Matrix -> Matrix
cdrColumn = map cdr


--Ex 2.40 ----------------------------------------------------------------------------------------------------------------
flatmap :: (a -> [b]) -> [a] -> [b]
flatmap f seq = foldl (\acc n -> acc ++ n) [] (map f seq)

uniquePairs :: Int -> [(Int, Int)]
uniquePairs n = flatmap (\i -> map (\j -> (i, j)) [1..i]) [1..n]

primeSumPairs :: Int -> [(Int, Int)]
primeSumPairs n = filter (\(i, j) -> isPrime $ i + j) (uniquePairs n)


--Ex 2.41 ----------------------------------------------------------------------------------------------------------------
uniqueTriples :: Int -> [(Int, Int, Int)]
uniqueTriples n = flatmap (\i -> flatmap (\j -> map (\k -> (i, j, k)) [1..j]) [1..i]) [1..n]

triplesLessSum :: Int -> Int -> [(Int, Int, Int)]
triplesLessSum n s = filter (\(i, j, k) -> i + j + k == s) (uniqueTriples n)


--Ex 2.42 ----------------------------------------------------------------------------------------------------------------
type Position = (Int, Int)

adjoinPosition :: Position -> [Position] -> [Position]
adjoinPosition x xs = x:xs

emptyBoard :: [Position]
emptyBoard = []

isSafe ::  Int -> [Position] -> Bool
isSafe _ [] = error "No position in set at column k"
isSafe k ((row, col):xs) | col == k  = safeHelper (row, col) xs 
                          | otherwise = isSafe k xs

safeHelper :: Position -> [Position] -> Bool
safeHelper _ [] = True
safeHelper (row, col) ((a, b):xs) | (row, col) == (a, b)            = safeHelper (row, col) xs
                                  |  row == a                       = False 
                                  |  col == b                       = False
                                  |  row + col == a + b             = False
                                  |  abs (row - col) == abs (a - b) = False 
                                  |  otherwise                      = safeHelper (row, col) xs


--Ex 2.44 ----------------------------------------------------------------------------------------------------------------
--We don't have access to the actual picture language, but we can still write functions as if we did

-- Define names so that the Haskell compiler will accept them
data Painter = Painter
beside x y = x
below x y = x

upSplit :: Painter -> Int -> Painter
upSplit painter n | n == 0    = painter
                   | otherwise = below painter (beside smaller smaller)

                     where smaller = upSplit painter (n - 1)


--Ex 2.45 ----------------------------------------------------------------------------------------------------------------
split :: (Painter -> Painter -> Painter) -> (Painter -> Painter -> Painter) -> (Painter -> Integer -> Painter)
split halver1 halver2 = let recc = \painter n -> let smaller = recc painter (n - 1) in
                                                     if (n == 0) then painter
                                                     else halver1 painter (halver2 smaller smaller) in recc


--Ex 2.46 ----------------------------------------------------------------------------------------------------------------
makeVec :: Double -> Double -> Vector
makeVec x y = [x, y]

xCorVect :: Vector -> Double
xCorVect [x, y] = x;

yCorVect :: Vector -> Double
yCorVect [x, y] = y;

addVect :: Vector -> Vector -> Vector
addVect vec1 vec2 = makeVec (xCorVect vec1 + xCorVect vec2) (yCorVect vec1 + yCorVect vec2)

subVect :: Vector -> Vector -> Vector
subVect vec1 vec2 = makeVec (xCorVect vec1 - xCorVect vec2) (yCorVect vec1 - yCorVect vec2)

scaleVect :: Vector -> Double -> Vector
scaleVect vec n = makeVec (xCorVect vec * n) (yCorVect vec * n) 


--Ex 2.47 ----------------------------------------------------------------------------------------------------------------
type Frame = [Vector]

getOrigin :: Frame -> Vector
getOrigin [origin, edge1, edge2] = origin

getEdge1 :: Frame -> Vector
getEdge1 [origin, edge1, edge2] = edge1

getEdge2 :: Frame -> Vector
getEdge2 [origin, edge1, edge2] = edge2

--Lisp's cons/cdr tree modeled as a binary tree using algebraic data types

data FrameCons = Atom Vector | Comp FrameCons FrameCons deriving (Read, Show, Eq)

getOriginCons :: FrameCons -> Vector
getOriginCons (Comp (Atom origin) _) = origin

getEdge1Cons :: FrameCons -> Vector
getEdge1Cons (Comp _ (Comp (Atom edge1) _)) = edge1

getEdge2Cons :: FrameCons -> Vector
getEdge2Cons (Comp _ (Comp _ (Atom edge2))) = edge2


--Ex 2.48 ----------------------------------------------------------------------------------------------------------------
data Seg = Seg (Vector, Vector) deriving (Ord, Eq)

makeSeg :: Vector -> Vector -> Seg
makeSeg v1 v2 = Seg (v1, v2)

startSeg :: Seg -> Vector 
startSeg (Seg (v1, _)) = v1

endSeg :: Seg -> Vector 
endSeg (Seg (_, v2)) = v2


--Ex 2.49 ----------------------------------------------------------------------------------------------------------------
segmentsToPainter :: [Seg] -> Painter
segmentsToPainter segList = Painter   

bottomLeft :: Vector
bottomLeft = makeVec 0 0

topLeft :: Vector
topLeft = makeVec 0 1

bottomRight :: Vector
bottomRight = makeVec 1 0

topRight :: Vector
topRight = makeVec 1 1 

leftMidpoint :: Vector
leftMidpoint = makeVec 0 0.5

rightMidpoint :: Vector
rightMidpoint = makeVec 1 0.5

topMidpoint :: Vector
topMidpoint = makeVec 0.5 1

bottomMidpoint :: Vector
bottomMidpoint = makeVec 0.5 0

--a
outline :: Painter
outline = segmentsToPainter [top, left, right, bottom]

          where top = makeSeg topLeft topRight
                bottom = makeSeg bottomLeft bottomRight
                left = makeSeg bottomLeft topLeft
                right = makeSeg bottomRight topRight

--b
cross :: Painter
cross = segmentsToPainter [posGrad, negGrad]

        where posGrad = makeSeg bottomLeft topRight
              negGrad = makeSeg bottomRight topLeft

--c
diamond :: Painter
diamond = segmentsToPainter [leftTop, rightTop, rightBottom, leftBottom]

          where leftTop = makeSeg leftMidpoint topMidpoint
                leftBottom = makeSeg leftMidpoint bottomMidpoint
                rightTop = makeSeg rightMidpoint topMidpoint
                rightBottom = makeSeg rightMidpoint bottomMidpoint

--Ex 2.50 ----------------------------------------------------------------------------------------------------------------
transformPainter :: Painter -> Vector -> Vector -> Vector -> Painter
transformPainter a b c d = a

flipHoriz :: Painter -> Painter
flipHoriz painter = transformPainter painter origin corner1 corner2

                     where origin = makeVec 1 0
                           corner1 = makeVec 0 0 
                           corner2 = makeVec 1 1

rotate180 :: Painter -> Painter
rotate180 painter = transformPainter painter origin corner1 corner2

                     where origin = makeVec 1 1
                           corner1 = makeVec 0 1 
                           corner2 = makeVec 1 0

rotate90 :: Painter -> Painter
rotate90 painter = transformPainter painter origin corner1 corner2

                     where origin = makeVec 0 1
                           corner1 = makeVec 0 0 
                           corner2 = makeVec 1 1


--Ex 2.51 ----------------------------------------------------------------------------------------------------------------
belowAbstracted :: Painter -> Painter -> Painter
belowAbstracted painter1 painter2 = rotate180 $ rotate90 $ beside (rotate90 painter1) (rotate90 painter2)


--Ex 2.52 ----------------------------------------------------------------------------------------------------------------
rightSplit :: Painter -> Int -> Painter
rightSplit painter n | n == 0    = painter
                     | otherwise = beside painter (below smaller smaller)

                     where smaller = upSplit painter (n - 1)


--b
cornerSplit :: Painter -> Int -> Painter
cornerSplit painter n | n == 0    = painter
                      | otherwise = beside (below painter up) (below up up)

                         where up = upSplit painter n 

--c
squareLimit :: Painter -> Int -> Painter
squareLimit = undefined

                
--Helper functions -------------------------------------------------------------------------------------------------------
-- In some cases I use car and cdr instead of pattern matching to keep my Haskell closer to the scheme code 
-- However in some cases it's just far more concise to pattern match

car :: [a] -> a
car = head

cdr :: [a] -> [a]
cdr = tail

cons :: a -> a -> [a]
cons a b = [a, b]

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..(n-1)], n `mod` x == 0]

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

average :: Double -> Double -> Double
average x y = (x + y) / 2.0

square :: (Num a) => a -> a
square a = a * a

floorLog :: Int -> Int -> Int
floorLog n val = floor $ logBase (fromIntegral n) (fromIntegral val)

