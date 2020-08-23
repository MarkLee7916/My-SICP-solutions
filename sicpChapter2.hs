import qualified Data.Char as C

--TODO 2.6, 2.35 2.52c 2.65

--I'm aware that pattern matching isn't really encouraged in the book as it couples together representation of data and use of data, 
--however in many cases it's far more concise and convienent than using selectors 

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
-- 'appendTree' stands in for Lisps 'append'. It's basically the ++ operator for the tree datatype

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


--Ex 2.54 ----------------------------------------------------------------------------------------------------------------
-- Quite an awkward question to do in Haskell, since that kind of behaviour is defined through typeclasses
-- However in scheme, you'd probably write a accumulate function that recurses whenever it hits a list
-- and has a accumulate condition of (acc = acc && n) so that any false value automatically makes the whole function return false


--Ex 2.56 ----------------------------------------------------------------------------------------------------------------
--building the symbol representation in Haskell
data Term = Number Int
          | Variable Char
          | Sum [Term]
          | Product [Term]
          | Power [Term] deriving (Eq, Show)

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

isSum :: Term -> Bool
isSum (Sum xs) = length xs > 1
isSum _ = False

isProduct :: Term -> Bool
isProduct (Product xs) = length xs > 1
isProduct _ = False

isNumber :: Term -> Bool
isNumber (Number _) = True
isNumber _ = False 

isPower :: Term -> Bool
isPower (Power _) = True
isPower _ = False 


makeSum :: Term -> Term -> Term
makeSum term1 term2 | term2 == Number 0                = term1
                    | term1 == Number 0                = term2
                    | isNumber term1 && isNumber term2 = makeNumber $ getNumber term1 + getNumber term2
                    | otherwise                        = Sum [term1, term2]
                        

makeProduct :: Term -> Term -> Term
makeProduct term1 term2 | term1 == Number 0 || term2 == Number 0 = Number 0
                        | term2 == Number 1                      = term1
                        | term1 == Number 1                      = term2
                        | isNumber term1 && isNumber term2       = makeNumber $ getNumber term1 * getNumber term2
                        | otherwise                              = Product [term1, term2]

makePower :: Term -> Term -> Term
makePower term1 term2 | term2 == Number 0                      = Number 1
                      | term2 == Number 1                      = term1
                      | isNumber term1 && isNumber term2       = makeNumber $ getNumber term1 ^ getNumber term2
                      | otherwise                              = Power [term1, term2]

makeNumber :: Int -> Term
makeNumber x = Number x

addend :: Term -> Term
addend (Sum terms) = car terms

augend :: Term -> Term
augend (Sum terms) | (length $ cdr terms) > 1 = Sum $ cdr terms
                   | otherwise                = car $ cdr terms

multiplier :: Term -> Term
multiplier (Product terms) = car terms

multiplicand :: Term -> Term
multiplicand (Product terms) | (length $ cdr terms) > 1 = Product $ cdr terms
                             | otherwise                =  car $ cdr terms

mantissa :: Term -> Term
mantissa (Power terms) = car terms

exponent_ :: Term -> Term
exponent_ (Power terms) = car $ cdr terms

getNumber :: Term -> Int
getNumber (Number x) = x

deriv :: Term -> Term -> Term
deriv exp var | isNumber exp   = Number 0
              | isVariable exp = if (exp == var) then Number 1 else Number 0
              | isSum exp      = makeSum (deriv (addend exp) var) (deriv (augend exp) var)  
              | isProduct exp  = makeSum (makeProduct (deriv (multiplier exp) var) (multiplicand exp)) (makeProduct (deriv (multiplicand exp) var) (multiplier exp))
              | isPower exp    = makeProduct (exponent_ exp) (makePower (mantissa exp) (makeSum (exponent_ exp) (Number (-1))))
              | otherwise      = error "Program can't detect the type of expression"


--Ex 2.57 ----------------------------------------------------------------------------------------------------------------
--Changed augend and multiplicand from `car $ cdr terms` to `if (length $ cdr terms) > 1 then Sum/Product $ cdr terms`
--i.e take the cdr of the rest of the list. If the cdr only has one element, just return what's inside the list, else
--return Sum/Product `cdr'd list`


--Ex 2.58 ----------------------------------------------------------------------------------------------------------------
-- This question doesn't map onto Haskell because we've chosen to represent the data using algebraic datatypes
-- It's not really possible to have something like 2 Product 3 Product 4 

--In the scheme/lisp implementation, I would modify the selectors/builders/predicates so that in an expression (term1 `operator` term2), 
    --term1 would correspond to (car expression)
    --term2 would corrsepond to (car $ cdr $ cdr expression)
    --`operator` would correspond to (car $ cdr expression)


--Ex 2.59 ----------------------------------------------------------------------------------------------------------------
data Set a = Set [a] deriving (Show)

elemOfSet :: (Eq a) => a -> Set a -> Bool
elemOfSet elem (Set xs) = foldl (\acc n -> acc || (n == elem)) False xs

adjoinSet :: (Eq a) => a -> Set a -> Set a
adjoinSet elem (Set xs) | not $ elemOfSet elem (Set xs) = Set (elem:xs)
                        | otherwise                     = Set xs

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet set1 (Set []) = set1
unionSet (Set (x:xs)) (Set ys) = unionSet (Set xs) (adjoinSet x (Set ys))


--Ex 2.60 ----------------------------------------------------------------------------------------------------------------
--Since joining is O(1) time as opposed to O(n) time, this will be more efficient for the operations seen here in terms of time complexity
--However a caveat is that over time this set could start to use up a lot of space if it's being used alot 

--Exact same function as for non-duplicate set
elemOfSetDup :: (Eq a) => a -> Set a -> Bool
elemOfSetDup = elemOfSet

--Done in O(1) time compared to O(n) time for non-duplicate set
adjoinSetDup :: (Eq a) => a -> Set a -> Set a
adjoinSetDup x (Set xs) = Set (x:xs)

--Done in O(n) time, however non-duplicate will do it in (O^2) time since it's adjoinSet is O(n) time 
--Also done using O(1) space. For non-duplicates it'll only use O(1) space if done using an iterative process in a tail recursive language
unionSetDup :: (Eq a) => Set a -> Set a -> Set a
unionSetDup (Set xs) (Set ys) = Set $ xs ++ ys

--Done in O(n^2) time either way
intersectionDup :: (Eq a) => Set a -> Set a -> Set a
intersectionDup (Set []) _ = Set []
intersectionDup _ (Set []) = Set []
intersectionDup (Set (x:xs)) (Set ys) | elemOfSetDup x (Set ys) = adjoinSetDup x (intersectionDup (Set xs) (Set ys))
                                      | otherwise               = (intersectionDup (Set xs) (Set ys))

--Summary of complexities:
    --No Duplicates
        --elemOfSet -> O(n)
        --adjoinSet -> O(n)
        --unionSet -> O(n^2)
        --intersection -> O(n^2)
    -- Duplicates
        --elemOfSet -> O(n)
        --adjoinSet -> O(1)
        --unionSet -> O(n)
        --intersection -> O(n^2)


--Sets as ordered lists ----------------------------------------------------------------------------------------------------------------
-- Elements in out set now need to be ordered -> makes our data structure less generic
-- If our list is always sorted we could also use binary search to bring our time complexity to O(nlogn)
-- My way of thinking about intersectionSet algorithm
    --If lists are sorted and car set1 < car set2, then car set1 must be the smaller than the smallest element in set2
    --Therefore it can't have any elements in set2 equal to it, therefore it can't be in the intersection

--Time complexity of intersectionSet
   --Each recursive call is only O(1) since the only operations are comparisons and cons
   --Since each recursive call eliminates either 1 or 2 elements from the lists, there can't be any more than n recursive calls
   --Therefore O(n)


--Ex 2.61, 2.62 ----------------------------------------------------------------------------------------------------------------
adjoinSetOrd :: (Ord a) => a -> Set a -> Set a
adjoinSetOrd elem (Set []) = Set [elem]
adjoinSetOrd elem (Set (x:xs)) | elem > x  = appendAtStart x (adjoinSetOrd elem (Set xs))
                               | elem == x = Set xs
                               | elem < x  = Set (elem:x:xs)

unionSetOrd :: (Ord a) => Set a -> Set a -> Set a
unionSetOrd (Set []) set2 = set2
unionSetOrd set1 (Set []) = set1 
unionSetOrd (Set (x:xs)) (Set (y:ys)) | x == y = appendAtStart x (unionSetOrd (Set xs) (Set ys))
                                      | x < y  = appendAtStart x (unionSetOrd (Set xs) (Set (y:ys)))
                                      | x > y  = appendAtStart y (unionSetOrd (Set (x:xs)) (Set ys))


-- Used an equivilant to cons when you can't use ':'
appendAtStart :: (Ord a) => a -> Set a -> Set a
appendAtStart elem (Set xs) = Set (elem:xs)


--Ex 2.63 ----------------------------------------------------------------------------------------------------------------
--a
    --The algorithms are both doing an inorder traversal of the trees
    --Therefore they both return the same list each time
    --They also return the same list for each example as they are all equivilant BSTs

--b
    --Both are recursing twice every stack frame
    --Much like traversing a binary tree, which is O(n)

    --However the first one is using append, which is O(n)
    --However, O(n^2) isn't the lowest bound
    --If we think about the size of the lists append is working on, it seems to be touching n elements every level in the tree
    --Since the number of levels in a balanced BST is equal to logn, if we only consider append algo is doing O(nlogn) working
    --Since the traversal is only O(n) in of itself, entire algorithm is O(nlogn)

    --Second one is just O(n) however


--Ex 2.64 ----------------------------------------------------------------------------------------------------------------
-- Essentially it's doing this

    --Take the middle element and make that the root of the tree
    --Make the left side of the root partialTree(everything left of the root in the list)
    --Make the right side of the root partialTree(everything right of the root in the list)
    --Since list is sorted, we know this will satisfy the main BST property, as everything left of root will be less than root and vice versa
   
    --Each recursive case will make its elements into a BST
    --This works because the BST property has closure: a BST is always made up of BSTs
    --i.e if a take a BST 1
    --                   / \
    --                  l   r
    
    --l and r and guaranteed to be BSTs themselves

--b
    --Since every recursion fixes one node, then it'll run in O(n) time


--Ex 2.65 ----------------------------------------------------------------------------------------------------------------
data BST a = Null | NodeBST a (BST a) (BST a) deriving (Eq, Read, Show)

--Chain together: flatten BSTs to sorted lists -> merge sorted lists -> build balanced BST from sorted list
--              : these operations all run in O(n) time, so overall will still run in O(n) time

--First, implement BST and sorted list conversion algorithms
treeToList :: (Eq a) => BST a -> [a]
treeToList tree = treeToListHelper tree []

treeToListHelper :: (Eq a) => BST a -> [a] -> [a]
treeToListHelper Null xs = xs
treeToListHelper (NodeBST root left right) xs = treeToListHelper left (root:(treeToListHelper right xs))

listToTree :: (Eq a) => [a] -> BST a
listToTree list = fst $ partialTree list (length list)

partialTree :: (Eq a) => [a] -> Int -> (BST a, [a])
partialTree list n 
  | n == 0        = (Null, list)
  | otherwise     = (NodeBST mid leftTree rightTree, remaining)

    where leftSize = (n - 1) `div` 2
          leftResult = partialTree list leftSize
          leftTree = fst leftResult

          nonLeftList = snd leftResult
          rightSize = n - leftSize + 1
          rightResult = partialTree (cdr nonLeftList) rightSize 
          rightTree = fst rightResult     
          remaining = snd rightResult        
             
          mid = car nonLeftList


--Ex 2.66 ----------------------------------------------------------------------------------------------------------------
data Record a = Record Int a 

instance Eq (Record a) where
   (Record x _) == (Record y _) = x == y

instance Ord (Record a) where
    (Record x _) `compare` (Record y _) = x `compare` y

key :: Record a -> Int
key (Record keyVal _) = keyVal

lookupSICP :: Int -> BST (Record a) -> Maybe (Record a)
lookupSICP _ Null = Nothing
lookupSICP givenKey (NodeBST (Record key vals) left right) 
  | givenKey == key = Just $ Record key vals
  | givenKey < key  = lookupSICP givenKey left
  | givenKey > key  = lookupSICP givenKey right


--Ex 2.67 ----------------------------------------------------------------------------------------------------------------
-- This is what the huffman tree looks like:
--
--         {A, B, C, D}
--        /            \
--       A(0)           {B, C, D}
--                     /         \
--                    B(10)       {C, D}
--                               /      \
--                              D(110)   C(111)

--Result: ADABBCA


--Ex 2.68 ----------------------------------------------------------------------------------------------------------------
data HuffTree = HuffLeaf Symbol Int | HuffNode HuffTree HuffTree [Symbol] Int deriving (Show)
type Bit = Char
type Symbol = String

instance Eq (HuffTree) where
    x == y = (getWeight x) == (getWeight y)

instance Ord (HuffTree) where
    x `compare` y = (getWeight x) `compare` (getWeight y)

getWeight :: HuffTree -> Int
getWeight (HuffLeaf _ weight) = weight
getWeight (HuffNode _ _ _ weight) = weight

encodeSymbol :: [Symbol] -> HuffTree -> [Bit]
encodeSymbol [] _ = []
encodeSymbol symbolList tree = foldr (\symbol list -> let bitList = getBitList symbol tree in (bitList ++ list)) [] xs

                             where xs = map (map C.toUpper) symbolList

getBitList :: Symbol -> HuffTree -> [Bit]
getBitList symbol (HuffNode left right xs weight) 
  | symbol `elem` xs = reverse $ getBitListHelper symbol (HuffNode left right xs weight) []
  | otherwise        = error "symbol not in huffman encoding tree"

getBitListHelper :: Symbol -> HuffTree -> [Bit] -> [Bit]
getBitListHelper symbol (HuffNode left right _ _) accum = getBitListHelper symbol left ('0':accum) ++ getBitListHelper symbol right ('1':accum)
getBitListHelper symbol (HuffLeaf leafSymbol _) accum 
  | symbol == leafSymbol = accum
  | otherwise            = []


--Ex 2.69 ----------------------------------------------------------------------------------------------------------------
successiveMerge :: [HuffTree] -> HuffTree
successiveMerge (tree:[]) = tree
successiveMerge (tree1:tree2:xs) = successiveMerge $ insertIntoOrderedList (mergeTwoNodes tree1 tree2) xs

mergeTwoNodes :: HuffTree -> HuffTree -> HuffTree
mergeTwoNodes (HuffLeaf symbolL weightL) (HuffLeaf symbolR weightR) = HuffNode leftNode rightNode [symbolL, symbolR] (weightL + weightR)

               where leftNode = HuffLeaf symbolL weightL
                     rightNode = HuffLeaf symbolR weightR
                        
mergeTwoNodes (HuffNode left right symbolsL weightL) (HuffLeaf symbolR weightR) = HuffNode leftNode rightNode (symbolsL ++ [symbolR]) (weightL + weightR)

               where leftNode = HuffNode left right symbolsL weightL
                     rightNode = HuffLeaf symbolR weightR

mergeTwoNodes (HuffLeaf symbolL weightL) (HuffNode left right symbolsR weightR) = HuffNode leftNode rightNode (symbolL:symbolsR) (weightL + weightR)

               where leftNode = HuffLeaf symbolL weightL
                     rightNode = HuffNode left right symbolsR weightR

mergeTwoNodes (HuffNode leftL rightL symbolsL weightL) (HuffNode leftR rightR symbolsR weightR) = HuffNode leftNode rightNode (symbolsL ++ symbolsR) (weightL + weightR)

               where leftNode = HuffNode leftL rightL symbolsL weightL
                     rightNode = HuffNode leftR rightR symbolsR weightR  

insertIntoOrderedList :: (Ord a) => a -> [a] -> [a]
insertIntoOrderedList elem [] = [elem]
insertIntoOrderedList elem (x:xs)
  | elem >= x = x:(insertIntoOrderedList elem xs)
  | otherwise = elem:x:xs


--Ex 2.70 ----------------------------------------------------------------------------------------------------------------
generateHuffmanTree :: [(Symbol, Int)] -> HuffTree
generateHuffmanTree = successiveMerge . makeLeafSet

makeLeafSet :: [(Symbol, Int)] -> [HuffTree]
makeLeafSet xs = foldl (\acc (symbol, freq) -> insertIntoOrderedList (HuffLeaf symbol freq) acc) [] xs

rockLyricsHuffTree :: HuffTree
rockLyricsHuffTree = generateHuffmanTree [("A", 2), ("GET", 2), ("SHA", 3), ("WAH", 1), ("BOOM", 1), ("JOB", 2), ("NA", 16), ("YIP", 9)]

encodedRockLyrics :: [Bit]
encodedRockLyrics = encodeSymbol ["Get","a","job","Sha","na","na","na","na","na","na","na","na","Get","a","job","Sha","na","na","na","na","na","na","na","na","Wah","yip","yip","yip","yip","yip","yip","yip","yip","yip","Sha","boom"] rockLyricsHuffTree

--My final string: 111111111011001110000000001111111110110011100000000011010101010101010101010111011011"
--My final length: 84
--Note that my string may be different from yours as the placement of 2 symbols of the same weight is an implementation detail

--Fixed length encoding:
    --Each symbol is 3 bits since we have 8 different symbols to decode
    --Message has 36 symbols
    --Therefore fixed length encoding would be 3 * 36 = 108 bits long if we ignore line breaks


--Ex 2.71 ----------------------------------------------------------------------------------------------------------------
--In general, most frequent symbol requires n - 1 bits
--In general, least frequent symbol requires 1 bit

--I'll sketch the tree for n = 3 for conciseness
--Let symbol-freq list equal {{A : 1}, {B : 2}, {C : 3}}

--                      {A, B, C}                          
--                     /         \
--                    C(0)        {B, C}
--                               /      \
--                              B(10)    C(11)    


--Ex 2.72 ----------------------------------------------------------------------------------------------------------------                              
--I think this question is asking the complexity of getBitList since it's only asking for specific symbols rather than lists of symbols

--Most frequent:
--    In a huffman tree the most frequent symbol is only one step away from the root, so it should just be O(1) time
--    However you also need to check if the symbol is in the tree first and you'll need to reverse it, which requires 2 full traversals
--    So it will be O(n) time. 

--Least frequent:
--    Least frequent symbols will be at the bottom, so will have to traverse entire tree to find them, so O(n) time. 
--    Since the check for the symbol being in the tree is only done once, the complexity is still O(n)
--    The ++ operator is O(n), however only one non empty list is actually generated, so the complexity is still O(n)


--Ex 2.73 ---------------------------------------------------------------------------------------------------------------- 
--a
--Instead of dispatching on type, we've used a mapping :: (function, operation type) -> action to perform on expression
--Our mapping depends on an operation type (i.e mult or add), number and variable aren't operation types. 

--b
sumDeriv :: Term -> Term -> Term
sumDeriv exp var = makeSum (deriv (addend exp) var) (deriv (augend exp) var)

prodDeriv :: Term -> Term -> Term
prodDeriv exp var = makeSum (makeProduct (deriv (multiplier exp) var) (multiplicand exp)) (makeProduct (deriv (multiplicand exp) var) (multiplier exp))
  
--c
--The scheme implementation is using global mutable state to maintain the mapping
--This isn't possible to write in Haskell as Haskell only has global immutable state

--d 
--Swap the key and value in the mapping


--Ex 2.74 ---------------------------------------------------------------------------------------------------------------- 
-- Earlier the book talks about stratospheric design, where we define higher level abstractions in term of lower level abstractions
-- I felt like it makes sense to think about accessing employees and accessing their records seperately
-- We'll need generic selectors that extract employee records regardless of which division they work in
-- We'll need more generic selectors that extract employee info regardless of which division they work in

-- We can think of this system as a layered system, where extracting employees is higher level than extracting employee info
-- We'll need two maps -> one for mapping a division onto a selector that extracts an employee from the set of employees
--                     -> another for mapping a division onto a selector that extracts a record from an employee

-- When it comes to extracting a type, the only information we'll need it which division we're currently in
-- We'll need to add a new entry in our mappings to incorporate the new division for each operation we want (in this case getSalary and getRecord)
         

--Ex 2.75 ----------------------------------------------------------------------------------------------------------------    
type ComplexMessage = String -> Double

realPart :: String
realPart = error "only written to get the example to compile"

imagPart :: String
imagPart = error "only written to get the example to compile"

magnitude :: String
magnitude = error "only written to get the example to compile"

angle :: String
angle = error "only written to get the example to compile"

makeFromMagAng :: Double -> Double -> ComplexMessage
makeFromMagAng mag ang  = \op ->
                            if (op == realPart) then mag * cos ang
                            else if (op == imagPart) then mag * sin ang              
                            else if (op == magnitude) then mag
                            else if (op == angle) then ang
                            else error "operation not supported by polar complex number"


--Ex 2.76 ----------------------------------------------------------------------------------------------------------------    
--In my opinion, adding a new entry to the mapping is violating modularity as we're modifying an already existing data structure

--Adding new types
    --Dispatch on type -> add new condition inside each generic selector function
    --Data directed    -> add new entry to mapping for each operation
    --Message passing  -> no changes needed in any already written module

    --Most appropiate would be message passing, as it requires no modification of previously written code
   

--Adding new operations
    --Dispatch on type -> no changes needed in any already written module
    --Data directed    -> add new entry to mapping for each representation
    --Message passing  -> add new operation onto every representation

    --Most appropiate would be dispatch on type, as it requires no modification of previously written code

--Overall, message passing and dispatch on type are both the best and worst depending on circumstance
--The data directed approach will be effective either way, but not the best either way


--Ex 2.77 ----------------------------------------------------------------------------------------------------------------  
--When (magnitude z) is called, (applyGeneric 'magnitude ('complex 'rectangular val)) is called
--applyGeneric extracts the first type tag (in this case 'complex) and uses it to retrieve a value from the mapping
--It uses the key 'magnitude '(complex), which calls magnitude again

--This time (applyGeneric 'magnitude ('rectangular val)) is called, 
--applyGeneric extracts the first type tag (in this case 'rectangular) and uses it to retrieve a value from the mapping
--This time it will actually executed the intended function, and you'll get back 5


--Ex 2.78 ----------------------------------------------------------------------------------------------------------------
--I'll write these out in pseudocode

--attachTag(type-tag, contents)
--    if (number? datum)
--        datum  
--    else
--       cons(type-tag, contents)

--typeTag(datum)
--    if (number? datum)
--        datum
--    else if (pair? datum)
--        car datum
--    else
--        error ("Bad tagged datum: TYPE-TAG", datum)

--contents(datum)
--    if (number? datum)
--        datum
--    else if (pair? datum)
--        cdr datum
--    else
--        error ("Bad tagged datum: CONTENTS", datum) 


--Ex 2.79 ----------------------------------------------------------------------------------------------------------------
--Again, I'll write these out in pseudocode
--This procedure will follow the same template as the other generic operations(i.e add, sub)

--Generic selector
--    equ?(x, y) = applyGeneric('equ?, x, y)

--Ordinary number package
--    put('equ? ('schemeNumber 'schemeNumber), eq?)

--Rational number package
--    eqRat?(x, y) = let mult = mulRat((makeRat(denom(x), numer(x)), y) in
--                              numer(mult) == 1 && denom(mult) == 1
--    put('equ? ('rational 'rational), (\x y -> eqRat?(x, y)))  

--Complex number package
--    eqComplex?(x, y) = eq?(realPart(x), realPart(y)) && eq?(imagPart(x), imagPart(y))
--    put('equ? ('complex 'complex), (\x y -> eqComplex?(x, y))) 


--Ex 2.80 ----------------------------------------------------------------------------------------------------------------
--Generic selector
--    zero?(x) = applyGeneric('zero?, x)

--Ordinary number package
--    put('zero? 'schemeNumber, (\x -> x == 0))

--Rational number package
--    zeroRat?(x) = numer x == 0
--    put('zero? 'rational, (\x -> zeroRat?(x)))  

--Complex number package
--    zeroComplex?(x) = realPart(x) == 0 && imagPart(x) == 0
--    put('zero? 'complex, (\x -> zeroComplex?(x))) 


--Ex 2.81 ----------------------------------------------------------------------------------------------------------------
--a 
--    It'll run in an infinite loop as it'll recurse on "apply-generic op (t1->t2 a1) a2)"
--    In the case where t1 and t2 are the same type, the recursion will have identical parameters to the original parameters, which causes the infinite loop

--b
--    It should already work out of the box, as the lookup will fail and throw an error that the operation isn't supported, which it must be
--    if it's reached that point in the function


--c
--    Simply add a conditional statement that automatically throws an error if type1 == type2
     

--Ex 2.82 ----------------------------------------------------------------------------------------------------------------
--Again, this'll have to be written in pseudocode as Haskell doesn't support global mutable state
--The code is slightly messy, however the main idea is if we can't find the procedure, call a recursive function
--that runs an iterative process that tries to convert everything to each available type. If any coercion fails, recursive on another type, else
--call applyGeneric again with the updated types

--applyGeneric(op, ...args):
--    let typeTags = args.map(typeTag)
--    let procedure = globalMapping.get(op, typetags)
--
--    if (procedure != null)
--        args.map(contents).apply(proc)  
--    else 
--         coerceArguments(op, typeTags, typeTags, args);
           
--coerceArguments(op, cooerceTypeTags, typeTags, args):
--    if (cooerceTypeTags == [])
--        throw an error    

--    let convertTypes = map((getCoercion (car coercerTypeTags)), typeTags)
--    if (!foldl((\acc type -> acc && (if (type == null) then False else True)), convertTypes, True))
--        coerceArguments(op, (cdr coercerTypeTags), typeTags, args)                 
--    else
--        applyGeneric(op, args.map(convertTypes)) 


--Ex 2.83 ----------------------------------------------------------------------------------------------------------------
data Rational = Rational Int Int
data ComplexPolar = ComplexPolar Double Double
data ComplexRect = ComplexRect Double Double
data Complex = ComplexPolar Double Double | ComplexRect Double Double 

regToRat :: Int -> Rational
regToRat x = Rational x 1

ratToReal :: Rational -> Double
ratToReal (Rational x y) = x / y

realToComplex :: Double -> Complex
realToComplex real = ComplexRect real 0

--We install a raise function for all the individial packages
--We then define a generic raise function in terms of applyGeneric
--i.e raise x = applyGeneric 'raise x


--Ex 2.84 ----------------------------------------------------------------------------------------------------------------
--applyGeneric is unchanged as we use a helper function

--applyGeneric(op, ...args):
--    let typeTags = args.map(typeTag)
--    let procedure = globalMapping.get(op, typetags)
--
--    if (procedure != null)
--        args.map(contents).apply(proc)  
--    else 
--         coerceArguments(op, typeTags, args);
       
--Helper functions -------------------------------------------------------------------------------------------------------
-- In some cases I use car and cdr instead of pattern matching to keep my Haskell code closer to the scheme code 
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

