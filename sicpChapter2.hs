
--TODO 2.6, 2.35

--Ex 2.1 -----------------------------------------------------------------------------------------------------------------

-- Raw data
data Pair = Pair Int Int deriving (Show, Eq)

-- API
numer :: Pair -> Int
numer (Pair x y) = x

denom :: Pair -> Int
denom (Pair x y) = y

make_rat :: Int -> Int -> Pair
make_rat n d = Pair numerator denominator

               where divisor = gcd n d
                     numerator = if ((n < 0) `xor` (d < 0)) then ((-1) * abs n) `div` divisor else (abs n) `div` divisor
                     denominator = (abs d) `div` divisor


--Ex 2.2 -----------------------------------------------------------------------------------------------------------------
-- Raw Data
data Point = Point Double Double deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show, Eq)

--API
make_point :: Double -> Double -> Point
make_point = Point 

x_point :: Point -> Double
x_point (Point x y) = x

y_point :: Point -> Double
y_point (Point x y) = y

make_segment :: Point -> Point -> Segment
make_segment x y = Segment x y

--Example of functions that use pattern matching. While the book uses selectors that provide a layer of abstraction over the underlying representation,
--I opted to use pattern matching in some cases because of how clean and concise it makes the code. Pattern matched code is really easy to reason about and find mistakes in.
--I
start_segment :: Segment -> Point
start_segment (Segment x y) = x

end_segment :: Segment -> Point
end_segment (Segment x y) = y

-- Example of a function that doesn't use pattern matching, much less concise
midpoint :: Segment -> Point
midpoint segment = make_point x_average y_average
                   
                   where x_average = (average (x_point (start_segment segment)) (x_point (end_segment segment)))
                         y_average = (average (y_point (start_segment segment)) (y_point (end_segment segment)))


--Ex 2.3 -----------------------------------------------------------------------------------------------------------------
--Raw data
data Rectangle = Rectangle Point Point Point Point deriving (Eq, Show)
data Rectangle_alt = Rectangle_alt Point Double Double deriving (Eq, Show)

--First implementation
make_rectangle :: Point -> Point -> Point -> Point -> Rectangle
make_rectangle = Rectangle

top_left_rectangle :: Rectangle -> Point
top_left_rectangle (Rectangle x y a b) = x

top_right_rectangle :: Rectangle -> Point
top_right_rectangle (Rectangle x y a b) = y

bottom_left_rectangle :: Rectangle -> Point
bottom_left_rectangle (Rectangle x y a b) = a

bottom_right_rectangle :: Rectangle -> Point
bottom_right_rectangle (Rectangle x y a b) = b

x_length :: Rectangle -> Double
x_length(Rectangle (Point i o) (Point q w) a b)  = abs (i - q) + abs (o - w) 

y_length :: Rectangle -> Double
y_length (Rectangle (Point i o) y (Point q w) b) = abs (i - q) + abs (o - w) 

--Second implementation
make_rectangle_alt :: Point -> Double -> Double -> Rectangle_alt
make_rectangle_alt = Rectangle_alt

x_length_alt :: Rectangle_alt -> Double
x_length_alt (Rectangle_alt _ x_size _) = x_size

y_length_alt :: Rectangle_alt -> Double
y_length_alt (Rectangle_alt _ _ y_size) = y_size

-- General abstract functions that apply to both implementations
perimeter :: Rectangle_alt -> Double
perimeter rect = (x_length_alt rect) * 2 + (y_length_alt rect) * 2

area :: Rectangle_alt -> Double
area rect = (x_length_alt rect) * (y_length_alt rect)


--Ex 2.4 -----------------------------------------------------------------------------------------------------------------
type EncodedDataInFunction = ((Int -> Int -> Int) -> Int)

cons_sicp :: Int -> Int -> EncodedDataInFunction
cons_sicp  x y = (\m -> m x y)

car_sicp  :: EncodedDataInFunction -> Int
car_sicp  c = c (\p q -> p)

cdr_sicp  :: EncodedDataInFunction -> Int
cdr_sicp  c = c (\p q -> q)


--Ex 2.5 -----------------------------------------------------------------------------------------------------------------
cons_alt :: Int -> Int -> Int
cons_alt a b = 2 ^ a * 3 ^ b

car_alt :: Int -> Int
car_alt y = if (is_root y 2) then (floor_log 2 y)            
            else car_alt (y `div` 3)

cdr_alt :: Int -> Int
cdr_alt y = if (is_root y 3) then (floor_log 3 y)            
            else cdr_alt (y `div` 2)

is_root x n = if (x == 1) then True 
              else if (x `mod` n /= 0) then False 
              else is_root (x `div` n) n

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
make_interval :: Double -> Double -> Interval
make_interval x y = Interval (min x y) (max x y)

lower_bound :: Interval -> Double
lower_bound (Interval a b) = a

upper_bound :: Interval -> Double
upper_bound (Interval a b) = b

width :: Interval -> Double
width (Interval x y) = 0.5 * (y - x)


--Ex 2.8 -----------------------------------------------------------------------------------------------------------------
sub_interval :: Interval -> Interval -> Interval
sub_interval (Interval l1 u1) (Interval l2 u2) = make_interval (l1 - u2) (u1 - l2)


--Ex 2.9 -----------------------------------------------------------------------------------------------------------------
mul_interval :: Interval -> Interval -> Interval
mul_interval (Interval l1 u1) (Interval l2 u2) = Interval (min_list combinations) (max_list combinations)

                                                 where combinations = [l1 * l2, l1 * u2, u1 * l2, u1 * u2]

div_interval :: Interval -> Interval -> Interval
div_interval x (Interval l2 u2) = mul_interval x (make_interval (1 / l2) (1 / u2))

--Example for mult, arguments are of the same width but returns different values
mul1 = mul_interval (Interval 2.5 3.6) (Interval 3.2 4.8)
mul2 = mul_interval (Interval 1.2 2.3) (Interval 1.9 3.5)
is_same_mul = width mul1 == width mul2

--Example for div, again arguments are of the same width but returns different values
div1 = div_interval (Interval 2.5 3.6) (Interval 3.2 4.8)
div2 = div_interval (Interval 1.2 2.3) (Interval 1.9 3.5)
is_same_div = width div1 == width div2


--Ex 2.10 ----------------------------------------------------------------------------------------------------------------
div_interval_zero_check :: Interval -> Interval -> Interval
div_interval_zero_check x (Interval l2 u2) = if (l2 == 0 || u2 == 0) then error "Can't divide an interval that spans zero"
                                             else mul_interval x (make_interval (1 / l2) (1 / u2))


--Ex 2.11 ----------------------------------------------------------------------------------------------------------------
mul_interval_cases :: Interval -> Interval -> Interval
mul_interval_cases (Interval l1 u1) (Interval l2 u2) | l1 < 0 && u1 < 0 && l2 < 0 && u2 < 0 = Interval (u1 * u2) (l1 * l2)
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
make_center_percent :: Double -> Double -> Interval 
make_center_percent center tolerance = make_interval lower_bound upper_bound

                                       where toleranceAsDecimal = tolerance / 100
                                             lower_bound = center - (toleranceAsDecimal * center)
                                             upper_bound = center + (toleranceAsDecimal * center)


--Ex 2.14 ----------------------------------------------------------------------------------------------------------------
add_interval :: Interval -> Interval -> Interval
add_interval (Interval l1 u1) (Interval l2 u2) = make_interval (l1 + l2) (u1 + u2)

par1 :: Interval -> Interval -> Interval
par1 r1 r2 = (mul_interval r1 r2) `div_interval` (add_interval r1 r2)

par2 :: Interval -> Interval -> Interval
par2 r1 r2 = one `div_interval` ((one `div_interval` r1) `add_interval` (one `div_interval` r2))

             where one = make_interval 1 1

is_equal_test1 :: Bool
is_equal_test1 = par1 (make_interval 9.9 10) (make_interval 11.7 12) == par2 (make_interval 9.9 10) (make_interval 11.7 12)

is_equal_test2 :: Bool
is_equal_test2 = par1 (make_interval 1 2) (make_interval 3 4) == par2 (make_interval 1 2) (make_interval 3 4)

is_equal_test3 :: Bool
is_equal_test3 = par1 (make_interval 9.99 10) (make_interval 11.78 12) == par2 (make_interval 9.99 10) (make_interval 11.78 12)


--Ex 2.17 ----------------------------------------------------------------------------------------------------------------
last_pair :: [a] -> a
last_pair xs = if (null xs) then error "No last element in an empty list"
               else if null $ cdr xs then car xs
               else last_pair $ cdr xs

--Ex 2.18 ----------------------------------------------------------------------------------------------------------------
reverse_sicp :: [a] -> [a]
reverse_sicp xs = if (null xs) then xs
                  else (reverse_sicp $ cdr xs) ++ [car xs]


--Ex 2.19 ----------------------------------------------------------------------------------------------------------------
first_denomination :: (Num a) => [a] -> a
first_denomination = car 

except_first_denomination :: (Num a) => [a] -> [a]
except_first_denomination = cdr

no_more :: (Num a) => [a] -> Bool
no_more = null


--Ex 2.20 ----------------------------------------------------------------------------------------------------------------
same_parity :: [Int] -> [Int]
same_parity xs = filter (\n -> n `mod` 2 == (car xs) `mod` 2) xs


--Ex 2.21 ----------------------------------------------------------------------------------------------------------------
square_list :: (Num a) => [a] -> [a]
square_list xs = if (null xs) then []
                 else (square $ car xs):(square_list $ cdr xs)

square_list_map :: (Num a) => [a] -> [a]
square_list_map = map (^2)


--Ex 2.23 ----------------------------------------------------------------------------------------------------------------
for_each [] _ = do 
             return ()

for_each xs f = do
             f $ car xs
             for_each (cdr xs) f


--Ex 2.27 ----------------------------------------------------------------------------------------------------------------
-- The Tree data type is essentially a stand in for Lisps 'list of lists' idea, which doesn't have a built-in equivalent in Haskell
-- 'append_tree' stands in for Lisps 'append'. It's basically Haskell's ++ operator for the tree datatype

data Tree a = Leaf a | Node [Tree a] deriving (Read, Show, Eq)

instance Functor Tree where 
    fmap f (Leaf a) = Leaf $ f a
    fmap _ (Node []) = Node []
    fmap f (Node (x:xs)) = (fmap f x) `append_tree` (fmap f $ Node xs)

deep_reverse :: Tree a -> Tree a
deep_reverse (Leaf a) = Leaf a
deep_reverse (Node []) = Node []
deep_reverse (Node (x:[])) = Node (x:[])
deep_reverse (Node (x:xs)) = (deep_reverse $ Node xs) `append_tree` (deep_reverse x)

append_tree :: Tree a -> Tree a -> Tree a
append_tree (Leaf x) (Leaf y) = Node ([Leaf x] ++ [Leaf y])
append_tree (Node xs) (Leaf y) = Node (xs ++ [Leaf y])
append_tree (Leaf x) (Node ys) = Node ((Leaf x):ys)
append_tree (Node xs) (Node ys) = Node (xs ++ ys)


--Ex 2.28 ----------------------------------------------------------------------------------------------------------------
fringe :: Tree a -> Tree a
fringe (Leaf a) = Leaf a
fringe (Node []) = Node []
fringe (Node (x:xs)) = x `append_tree` (fringe $ Node xs)


--Ex 2.29 ----------------------------------------------------------------------------------------------------------------
-- I decided to define the data structures the same way the book describes it. It ends up being quite awkward but it works
data BinaryMobile = BinaryMobile Branch Branch deriving (Eq, Read, Show)
data Branch = RecursiveMobile Int BinaryMobile | Weight Int Int deriving (Eq, Read, Show)

--Selectors
make_mobile :: Branch -> Branch -> BinaryMobile
make_mobile left right = BinaryMobile left right

left_branch :: BinaryMobile -> Branch
left_branch (BinaryMobile x y) = x

right_branch :: BinaryMobile -> Branch
right_branch (BinaryMobile x y) = y

branch_length :: Branch -> Int
branch_length (RecursiveMobile length _) = length
branch_length (Weight length _) = length

branch_structure :: Branch -> (Either Int BinaryMobile)
branch_structure (Weight _ val) = Left val
branch_structure (RecursiveMobile _ mobile) = Right mobile

--Compute functions
total_weight_mobile :: BinaryMobile -> Int
total_weight_mobile (BinaryMobile left right) = total_weight_branch left + total_weight_branch right

total_weight_branch :: Branch -> Int
total_weight_branch (RecursiveMobile _ mobile) = total_weight_mobile mobile
total_weight_branch (Weight _ val) = val

is_balanced :: BinaryMobile -> Bool
is_balanced (BinaryMobile left right) = branch_torque left == branch_torque right && is_balanced_branch left && is_balanced_branch right

is_balanced_branch :: Branch -> Bool
is_balanced_branch (RecursiveMobile length mobile) = is_balanced mobile
is_balanced_branch (Weight _ _) = True

branch_torque :: Branch -> Int
branch_torque (RecursiveMobile length mobile) = length * (mobile_torque mobile)
branch_torque (Weight length value) = length * value

mobile_torque :: BinaryMobile -> Int
mobile_torque (BinaryMobile left right) = branch_torque left + branch_torque right


--Ex 2.30 ----------------------------------------------------------------------------------------------------------------
square_tree :: (Num a) => Tree a -> Tree a
square_tree (Leaf a) = Leaf $ square a
square_tree (Node []) = Node []
square_tree (Node (x:xs)) = (square_tree x) `append_tree` (square_tree $ Node xs)


--Ex 2.31 ----------------------------------------------------------------------------------------------------------------
tree_map :: Tree a -> (a -> b) -> Tree b
tree_map (Leaf a) f = Leaf $ f a
tree_map (Node []) f = Node []
tree_map (Node (x:xs)) f = (tree_map x f) `append_tree` (tree_map (Node xs) f)


--Ex 2.32 ----------------------------------------------------------------------------------------------------------------
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = foldl (\acc n -> (x:n):acc) (powerset xs) (powerset xs)


--Ex 2.33 ----------------------------------------------------------------------------------------------------------------
map_sicp :: (a -> b) -> [a] -> [b]
map_sicp f = foldr (\n acc -> (f n):acc) [] 

append :: [a] -> [a] -> [a]
append xs ys = foldr (\n acc -> n:acc) ys xs

length_sicp :: [a] -> Int
length_sicp = foldl (\acc n -> acc + 1) 0


--Ex 2.34 ----------------------------------------------------------------------------------------------------------------
horner_eval :: Int -> [Int] -> Int
horner_eval x = foldr (\n acc -> (acc * x) + n) 0 


--Ex 2.35 ----------------------------------------------------------------------------------------------------------------
count_leaves :: Tree a -> Int
count_leaves tree = undefined


--Ex 2.36 ----------------------------------------------------------------------------------------------------------------
accumulate_n :: (a -> a -> a) -> a -> [[a]] -> [a]
accumulate_n f start_val seq = if (null $ car seq) then []
                               else (foldl f start_val get_first_elems):(accumulate_n f start_val get_rest)

                               where get_first_elems = map car seq
                                     get_rest = map cdr seq

--Ex 2.37 ----------------------------------------------------------------------------------------------------------------
type Vector = [Int]
type Matrix = [[Int]]

matrix_vector_prod :: Matrix -> Vector -> Vector
matrix_vector_prod [] vector = []
matrix_vector_prod (row:matrix) vector = (foldl (+) 0 terms):(matrix_vector_prod matrix vector)

                                            where terms = zipWith (*) row vector 

matrix_prod :: Matrix -> Matrix -> Matrix
matrix_prod [] _ = []
matrix_prod m n = (matrix_prod_row m n):(matrix_prod (cdr m) n)

dot_prod :: Vector -> Vector -> Int
dot_prod [] _ = 0
dot_prod _ [] = 0
dot_prod v1 v2 = foldl (+) 0 (zipWith (*) v1 v2) 

transpose :: Matrix -> Matrix
transpose [] = []
transpose ([]:m) = []
transpose m = (car_column m):(transpose $ cdr_column m)


--Ex 2.39 ----------------------------------------------------------------------------------------------------------------
reverse_foldr :: [a] -> [a]
reverse_foldr xs = foldr (\n acc -> acc ++ [n]) [] xs

reverse_foldl :: [a] -> [a]
reverse_foldl xs = foldl (\acc n -> n:acc) [] xs


--Helper functions
matrix_prod_row :: Matrix -> Matrix -> Vector
matrix_prod_row _ [] = []
matrix_prod_row _ ([]:n) = []
matrix_prod_row m n = (dot_prod (car m) (car_column n)):(matrix_prod_row m (cdr_column n))

car_column :: Matrix -> Vector
car_column = map car

cdr_column :: Matrix -> Matrix
cdr_column = map cdr


--Helper functions -------------------------------------------------------------------------------------------------------
-- In some cases I use car and cdr instead of pattern matching to keep my Haskell closer to the scheme code 
car = head
cdr = tail

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

average :: Double -> Double -> Double
average x y = (x + y) / 2.0

square :: (Num a) => a -> a
square a = a * a

floor_log :: Int -> Int -> Int
floor_log n val = floor $ logBase (fromIntegral n) (fromIntegral val)

min_list :: (Ord a) => [a] -> a
min_list = foldl1 (\acc n -> min acc n) 

max_list :: (Ord a) => [a] -> a
max_list = foldl1 (\acc n -> max acc n) 
