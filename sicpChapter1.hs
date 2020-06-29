import Data.Bits

-- Ex 1.3
sumOfSquaresGreatestTwo :: Int -> Int -> Int -> Int
sumOfSquaresGreatestTwo x y z | x <= y && x <= z = sumOfSquares y z
                              | y <= x && y <= z = sumOfSquares x z
                              | z <= x && z <= y = sumOfSquares x y

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = square x + square y


-- Ex 1.7
sqrtSicp :: Double -> Double
sqrtSicp x = sqrtHelp x 1 0

sqrtHelp :: Double -> Double -> Double -> Double
sqrtHelp x guess oldGuess = if (goodEnough guess oldGuess) then guess
                            else sqrtHelp x improvedGuess guess

                            where improvedGuess = average guess (x / guess)


goodEnough :: Double -> Double -> Bool
goodEnough guess oldGuess = if (abs (guess - oldGuess) <= tolerance) then True 
                            else False

                            where tolerance = 0.0000000001

-- Ex 1.8
cubeRootSicp :: Double -> Double
cubeRootSicp  x = cubeRootHelp x 1 0

cubeRootHelp :: Double -> Double -> Double -> Double
cubeRootHelp x guess oldGuess = if (goodEnough guess oldGuess) then guess
                                else cubeRootHelp x improvedGuess guess

                                where improvedGuess = (x / (guess ^ 2) + (2 * guess)) / 3

-- Ex 1.11
fRecurse :: Int -> Int
fRecurse n | n < 3     = n 
            | otherwise = fRecurse (n - 1) + 2 * fRecurse (n - 2) + 3 * fRecurse (n - 3)

fIter :: Int -> Int
fIter n = fIterHelper 2 1 0 (n - 2)

fIterHelper :: Int -> Int -> Int -> Int -> Int
fIterHelper a b c count | count < 0  = count + 2
                        | count == 0 = a 
                        | otherwise  = fIterHelper (a + 2 * b + 3 * c) a b (count - 1)


-- Ex 1.12
pascalTriangle :: Int -> Int -> Int
pascalTriangle r c | outOfBounds    = error "Invalid arguments" 
                   | edgeOfTriangle = 1 
                   | otherwise        = pascalTriangle (r - 1) (c - 1) + pascalTriangle (r - 1) c

                   where outOfBounds = r < 1 || c < 1 || r < c
                         edgeOfTriangle = c == 1 || r == c

-- Ex 1.16
fastPowerIter :: Int -> Int -> Int
fastPowerIter mantissa exp = fastPowerIterHelper 1 mantissa exp

fastPowerIterHelper :: Int -> Int -> Int -> Int
fastPowerIterHelper total helper exp | exp == 0  = total
                                     | even exp  = recurseHalfingExponent
                                     | otherwise = recurseDecrementing

                                       where recurseHalfingExponent = fastPowerIterHelper total (helper ^ 2) (half exp)
                                             recurseDecrementing = fastPowerIterHelper (total * helper) helper (exp - 1)
-- Ex 1.17
fastMultRecurse :: Int -> Int -> Int
fastMultRecurse x y | x == 0 || y == 0 = 0
                    | x == 1           = y
                    | y == 1           = x
                    | even y           = recurseHalfingSecondArg
                    | even x           = recurseHalfingFirstArg
                    | otherwise        = recurseDecrementingSecondArg

                    where recurseHalfingSecondArg = 2 * fastMultRecurse x (half y)
                          recurseHalfingFirstArg = 2 * fastMultRecurse (half x) y
                          recurseDecrementingSecondArg = x + fastMultRecurse x (y - 1)


-- Ex 1.18
fastMultIter :: Int -> Int -> Int
fastMultIter x y = multHelp 0 x y

multHelp :: Int -> Int -> Int -> Int
multHelp helper x y = if (x == 0 || y == 0) then 0
                      else if (x == 1) then y + helper
                      else if (y == 1) then x
                      else if (even x) then recurseHalfing
                      else recurseDecrementing

                      where recurseHalfing = multHelp helper (half x) (double y) 
                             recurseDecrementing = multHelp  (helper + y) (x - 1) y


-- Ex 1.19
fib :: Int -> Int
fib n = fibIter 1 0 0 1 n

fibIter :: Int -> Int -> Int -> Int -> Int -> Int
fibIter a b p q count | (count == 0) = b
                      | (even count) = fibIter a b (p * p + q * q) (2 * p * q + q * q) (half count)
                      | otherwise    = fibIter (b * q + a * q + a * p) (b * p + a * q) p q (count - 1)


-- Ex 1.23
isPrime :: Int -> Bool
isPrime n = n == smallestDivisor n

smallestDivisor :: Int -> Int
smallestDivisor n = smalldivHelper n 2 (floorSqrt n)

smalldivHelper :: Int -> Int -> Int -> Int
smalldivHelper n curr lim = if (curr > lim) then n
                            else if (n `mod` curr == 0) then curr
                            else smalldivHelper n (next curr) lim

next :: Int -> Int
next n = if (n == 2) then 3 else n + 2


--Ex 1.27
fermatTest :: Int -> Bool
fermatTest n = fermatHelper (n - 1) n

fermatHelper :: Int  -> Int  -> Bool
fermatHelper a n = if (a == 1) then True
                   else if ((expmod a n n) `mod` n /= a) then False
                   else fermatHelper (a - 1) n

expmod :: Int -> Int -> Int -> Int
expmod base exp m = if (exp == 0) then 1
                    else if (even exp) then (square $ expmod base (exp `div` 2) m) `mod` m 
                    else base * (expmod base (exp - 1) m) `mod` m 


--Ex 1.29
simpsons :: (Double -> Double) -> Double -> Double -> Double -> Double
simpsons f a b n = let h = (b - a) / n in (simpsonsHelper f a b n 0 h) * h / 3

simpsonsHelper :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double -> Double
simpsonsHelper f a b n k h   | k == n           = yk
                             | k == 0           = yk + recurse
                             | (even $ round k) = 2 * yk + recurse
                             | otherwise        = 4 * yk + recurse

                             where yk = f (a + k * h)
                                   recurse = simpsonsHelper f a b n (k + 1) h


--Ex 1.30
sumSicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
sumSicp term a next b = sumIter term a next b 0

sumIter :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int -> Int
sumIter term a next b total = if (a > b) then total
                              else sumIter term (next a) next b (total + (term a))


--Ex 1.31a
productSicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
productSicp term a next b = if (a == b) then 0 
                            else productIter term a next b 1


productIter :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int -> Int
productIter term a next b total = if (a > b) then total
                                  else productIter term (next a) next b (total * (term a))


fac :: Int -> Int
fac x = productSicp (\n -> n) 1 (\n -> n + 1)  x


piSicp :: Double 
piSicp = 4.0 * top / (bottom * limit)

          where limit = 20
                top = fromIntegral $ productSicp (\n -> if (n == 2) then n else if (even n) then n * n else 1) 2 (+1) (floor limit)
                bottom = fromIntegral $ productSicp (\n -> if (even n) then 1 else n * n) 2 (+1) (floor limit)


--Ex 1.31b
productRecSicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
productRecSicp term a next b = if (a > b) then 1 
                                else term a * productRecSicp term (next a) next b


--Ex 1.32a
accum :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accum combiner nullValue term a next b = if (a > b) then nullValue
                                         else combiner (term a) (accum combiner nullValue term (next a) next b)

prodAccum term a next b = accum (*) 1 term a next b
sumAccum term a next b = accum (+) 0 term a next b


--Ex 1.32b
accumIter :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accumIter combiner nullValue term a next b = accumIterHelper combiner nullValue term a next b

accumIterHelper :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accumIterHelper combiner total term a next b = if (a > b) then total
                                               else accumIterHelper combiner (combiner total (term a)) term (next a) next b 


--Ex 1.33
filterAccum :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> (o -> Bool) -> a
filterAccum combiner nullValue term a next b pred = filterAccumHelper combiner nullValue term a next b pred

filterAccumHelper :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> (o -> Bool) -> a
filterAccumHelper combiner total term a next b pred = if (a > b) then total
                                                      else if (pred a) then filterAccumHelper combiner (combiner total (term a)) term (next a) next b pred
                                                      else filterAccumHelper combiner total term (next a) next b pred


--Ex 1.33a
sumOfSquaresOfPrimes :: Int -> Int -> Int
sumOfSquaresOfPrimes a b = filterAccum (+) 0 square a (+1) b isPrime


--Ex 1.33b
integersRelativelyPrimeProduct :: Int -> Int
integersRelativelyPrimeProduct n = filterAccum (*) 1 (+0) 1 (+1) n (relativePrime n)


--Ex 1.35
fixedPoint :: (a -> a) -> a -> (a -> a -> Bool) -> a 
fixedPoint f x closeEnough = if (closeEnough x next) then next
                             else fixedPoint f next closeEnough

                             where next = f x

closeEnough :: Double -> Double -> Bool
closeEnough guess nextGuess = let accuracyLimit = 0.00001 in abs (guess - nextGuess) < accuracyLimit

goldenRatio :: Double
goldenRatio = fixedPoint (\x -> 1.0 + (1.0 / x)) 1 closeEnough


--Ex 1.37
contFrac :: Double -> Double -> Int -> Double
contFrac n d k = if (k == 1) then n / d
                 else n / (d + contFrac n d (k - 1))

goldenRatioFrac :: Double
goldenRatioFrac = let repetitions = 10000 in 1 / contFrac 1 1 repetitions


--Ex 1.37b
contFracIter :: Double -> Double -> Int -> Double
contFracIter n d k = fracHelper n d k 0.0

fracHelper :: Double -> Double -> Int -> Double -> Double
fracHelper n d k total = if (k == 1) then total
                         else fracHelper n d (k - 1) (n / (total + d))


--Ex 1.38, modified to deal with list inputs rather than fixed values
type Series = [Double]

contFracList :: Series -> Series -> Int -> Double
contFracList (ni:n) (di:d) k = if (k == 1) then ni / di
                               else ni / (di + contFracList n d (k - 1))

generateSequence :: Int -> Double -> Series
generateSequence counter add = if (counter `mod` 3 == 0) then add:(generateSequence (counter + 1) (add + 2))
                               else 1:(generateSequence (counter + 1) add)

di :: Series
di = 1:(generateSequence 0 2)

eSicp :: Double
eSicp = let repetitions = 10000 in contFracList (cycle [1]) di repetitions + 2


--Ex 1.39, modified to be more generic, have to pass in a function to decide how to combine fractions
contFracListGeneric :: Series -> Series -> (Double -> Double -> Double) -> Int -> Double
contFracListGeneric (ni:n) (di:d) f k = if (k == 1) then ni / di
                                        else ni / (f di (contFracListGeneric n d f (k - 1)))

tanApprox :: Double -> Double
tanApprox x = contFracListGeneric topSequence [1, 3..] (-) 10000

               where topSequence = x:(cycle $ [x ^ 2])


--Ex 1.40
cubic :: Double -> Double -> Double -> (Double -> Double)
cubic a b c = (\x -> x ^ 3 + (a * x ^ 2) + (b * x) + c)


--Ex 1.41, could also just use Haskell's operator i.e doubleSicp f = f . f
doubleSicp :: (a -> a) -> (a -> a)
doubleSicp f = (\n -> f $ f n) 


--Ex 1.42, again could also just use Haskell's operator i.e compose f g = f . g
compose :: (a -> c) -> (b -> a) -> (b -> c)
compose f g = (\n -> f $ g n)


--Ex 1.43
repeated :: (a -> a) -> Int -> (a -> a)
repeated f n = repeatedHelper f f n

repeatedHelper :: (a -> a) -> (a -> a) -> Int -> (a -> a)
repeatedHelper f f' n | n == 1    = f'
                      | otherwise = repeatedHelper f (f . f') (n - 1)


--Ex 1.44
smooth :: (Double -> Double) -> (Double -> Double)
smooth f = (\x -> let dx = 0.05 in (f(x - dx) + f x + f (x + dx)) / 3)

nSmooth ::  Int -> (Double -> Double) -> (Double -> Double)
nSmooth = repeated smooth


--Ex 1.45
nthRoot :: Int -> Double -> Double
nthRoot n x = fixedPoint (multDampen rootFunction) x goodEnough

               where multDampen = repeated averageDamp dampsNeeded
                     rootFunction = (\y -> x / (y ^ (n - 1)))
                     dampsNeeded = floorLog 2 n

averageDamp :: (Double -> Double) -> (Double -> Double)
averageDamp f = (\n -> (f n + n) / 2)


--Ex 1.46
iterativeImprove :: (a -> Bool) -> (a -> a) -> (a -> a)
iterativeImprove goodEnough improve = (\guess -> if (goodEnough guess) then guess else iterativeImprove goodEnough improve (improve guess)) 

sqrtIterImprove :: Double -> Double
sqrtIterImprove x = (iterativeImprove goodEnoughFunc improveFunc) 1

                      where goodEnoughFunc = (\guess -> let threshold = 0.00001 in abs(x - guess ^ 2) < threshold)
                            improveFunc = (\guess -> average guess (x / guess))

fixedPointImprove :: (a -> a) -> a -> (a -> Bool) -> a 
fixedPointImprove f x goodEnough = (iterativeImprove goodEnough f) x


-- Some helper functions
relativePrime :: Int -> Int -> Bool
relativePrime n a = gcd n a == 1

double :: Int -> Int
double n = shift n 1

half :: Int -> Int
half n = shift n (-1)

average :: Double -> Double -> Double
average x y = (x + y) / 2.0

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

square :: (Num a) => a -> a
square x = x * x

floorLog :: Int -> Int -> Int
floorLog n val = floor $ logBase (fromIntegral n) (fromIntegral val)

inc :: Int -> Int
inc x = x + 1









 




