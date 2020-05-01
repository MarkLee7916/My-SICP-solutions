import Data.Bits

-- Ex 1.3
sum_of_squares_greatest_two :: Int -> Int -> Int -> Int
sum_of_squares_greatest_two x y z | x <= y && x <= z = sum_of_squares y z
                                  | y <= x && y <= z = sum_of_squares x z
                                  | z <= x && z <= y = sum_of_squares x y

sum_of_squares :: Int -> Int -> Int
sum_of_squares x y = square x + square y


-- Ex 1.7
sqrt_sicp :: Double -> Double
sqrt_sicp x = sqrt_help x 1 0

sqrt_help :: Double -> Double -> Double -> Double
sqrt_help x guess old_guess = if (good_enough guess old_guess) then guess
                              else sqrt_help x improved_guess guess

                              where improved_guess = average guess (x / guess)


good_enough :: Double -> Double -> Bool
good_enough guess old_guess = if (abs (guess - old_guess) <= tolerance) then True 
                              else False

                              where tolerance = 0.0000000001

-- Ex 1.8
cube_root_sicp :: Double -> Double
cube_root_sicp  x = cube_root_help x 1 0

cube_root_help :: Double -> Double -> Double -> Double
cube_root_help x guess old_guess = if (good_enough guess old_guess) then guess
                                   else cube_root_help x improved_guess guess

                                   where improved_guess = (x / (guess ^ 2) + (2 * guess)) / 3

-- Ex 1.11
f_recurse :: Int -> Int
f_recurse n | n < 3     = n 
            | otherwise = f_recurse (n - 1) + 2 * f_recurse (n - 2) + 3 * f_recurse (n - 3)

f_iter :: Int -> Int
f_iter n = f_iter_helper 2 1 0 (n - 2)

f_iter_helper :: Int -> Int -> Int -> Int -> Int
f_iter_helper a b c count | count < 0  = count + 2
                          | count == 0 = a 
                          | otherwise  = f_iter_helper (a + 2 * b + 3 * c) a b (count - 1)


-- Ex 1.12
pascal_triangle :: Int -> Int -> Int
pascal_triangle r c | out_of_bounds    = error "Invalid arguments" 
                    | edge_of_triangle = 1 
                    | otherwise        = pascal_triangle (r - 1) (c - 1) + pascal_triangle (r - 1) c

                      where out_of_bounds = r < 1 || c < 1 || r < c
                            edge_of_triangle = c == 1 || r == c

-- Ex 1.16
fast_power_iter :: Int -> Int -> Int
fast_power_iter mantissa exp = fast_power_iter_helper 1 mantissa exp

fast_power_iter_helper :: Int -> Int -> Int -> Int
fast_power_iter_helper total helper exp | exp == 0  = total
                                        | even exp  = recurse_halfing_exponent
                                        | otherwise = recurse_decrementing

                                          where recurse_halfing_exponent = fast_power_iter_helper total (helper ^ 2) (half exp)
                                                recurse_decrementing = fast_power_iter_helper (total * helper) helper (exp - 1)
-- Ex 1.17
fast_mult_recurse :: Int -> Int -> Int
fast_mult_recurse x y | x == 0 || y == 0 = 0
                      | x == 1           = y
                      | y == 1           = x
                      | even y           = recurse_halfing_second_arg
                      | even x           = recurse_halfing_first_arg
                      | otherwise        = recurse_decrementing_second_arg

                        where recurse_halfing_second_arg = 2 * fast_mult_recurse x (half y)
                              recurse_halfing_first_arg = 2 * fast_mult_recurse (half x) y
                              recurse_decrementing_second_arg = x + fast_mult_recurse x (y - 1)


-- Ex 1.18
fast_mult_iter :: Int -> Int -> Int
fast_mult_iter x y = mult_help 0 x y

mult_help :: Int -> Int -> Int -> Int
mult_help helper x y = if (x == 0 || y == 0) then 0
                       else if (x == 1) then y + helper
                       else if (y == 1) then x
                       else if (even x) then recurse_halfing
                       else recurse_decrementing

                       where recurse_halfing = mult_help helper (half x) (double y) 
                             recurse_decrementing = mult_help  (helper + y) (x - 1) y


-- Ex 1.19
fib :: Int -> Int
fib n = fib_iter 1 0 0 1 n

fib_iter :: Int -> Int -> Int -> Int -> Int -> Int
fib_iter a b p q count | (count == 0) = b
                       | (even count) = fib_iter a b (p * p + q * q) (2 * p * q + q * q) (half count)
                       | otherwise    = fib_iter (b * q + a * q + a * p) (b * p + a * q) p q (count - 1)


-- Ex 1.23
is_prime :: Int -> Bool
is_prime n = n == smallest_divisor n

smallest_divisor :: Int -> Int
smallest_divisor n = smalldiv_helper n 2 (floor_sqrt n)

smalldiv_helper :: Int -> Int -> Int -> Int
smalldiv_helper n curr lim = if (curr > lim) then n
                             else if (n `mod` curr == 0) then curr
                             else smalldiv_helper n (next curr) lim

next :: Int -> Int
next n = if (n == 2) then 3 else n + 2


--Ex 1.27
fermat_test :: Int -> Bool
fermat_test n = fermat_helper (n - 1) n

fermat_helper :: Int  -> Int  -> Bool
fermat_helper a n = if (a == 1) then True
                    else if ((expmod a n n) `mod` n /= a) then False
                    else fermat_helper (a - 1) n

expmod :: Int -> Int -> Int -> Int
expmod base exp m = if (exp == 0) then 1
                    else if (even exp) then (square $ expmod base (exp `div` 2) m) `mod` m 
                    else base * (expmod base (exp - 1) m) `mod` m 


--Ex 1.29
simpsons :: (Double -> Double) -> Double -> Double -> Double -> Double
simpsons f a b n = let h = (b - a) / n in (simpsons_helper f a b n 0 h) * h / 3

simpsons_helper :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double -> Double
simpsons_helper f a b n k h   | k == n           = yk
                              | k == 0           = yk + recurse
                              | (even $ round k) = 2 * yk + recurse
                              | otherwise        = 4 * yk + recurse

                              where yk = f (a + k * h)
                                    recurse = simpsons_helper f a b n (k + 1) h


--Ex 1.30
sum_sicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
sum_sicp term a next b = sum_iter term a next b 0

sum_iter :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int -> Int
sum_iter term a next b total = if (a > b) then total
                               else sum_iter term (next a) next b (total + (term a))


--Ex 1.31a
product_sicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
product_sicp term a next b = if (a == b) then 0 
                             else product_iter term a next b 1


product_iter :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int -> Int
product_iter term a next b total = if (a > b) then total
                                   else product_iter term (next a) next b (total * (term a))


fac :: Int -> Int
fac x = product_sicp (\n -> n) 1 (\n -> n + 1)  x


pi_sicp :: Double 
pi_sicp = 4.0 * top / (bottom * limit)

          where limit = 20
                top = fromIntegral $ product_sicp (\n -> if (n == 2) then n else if (even n) then n * n else 1) 2 (+1) (floor limit)
                bottom = fromIntegral $ product_sicp (\n -> if (even n) then 1 else n * n) 2 (+1) (floor limit)


--Ex 1.31b
product_rec_sicp :: (Int -> Int) -> Int -> (Int -> Int) -> Int -> Int
product_rec_sicp term a next b = if (a > b) then 1 
                                 else term a * product_rec_sicp term (next a) next b


--Ex 1.32a
accum :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accum combiner null_value term a next b = if (a > b) then null_value
                                          else combiner (term a) (accum combiner null_value term (next a) next b)

prod_accum term a next b = accum (*) 1 term a next b
sum_accum term a next b = accum (+) 0 term a next b


--Ex 1.32b
accum_iter :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accum_iter combiner null_value term a next b = accum_iter_helper combiner null_value term a next b

accum_iter_helper :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> a
accum_iter_helper combiner total term a next b = if (a > b) then total
                                                 else accum_iter_helper combiner (combiner total (term a)) term (next a) next b 


--Ex 1.33
filter_accum :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> (o -> Bool) -> a
filter_accum combiner null_value term a next b pred = filter_accum_helper combiner null_value term a next b pred

filter_accum_helper :: (Ord o) => (a -> a -> a) -> a -> (o -> a) -> o -> (o -> o) -> o -> (o -> Bool) -> a
filter_accum_helper combiner total term a next b pred = if (a > b) then total
                                                        else if (pred a) then filter_accum_helper combiner (combiner total (term a)) term (next a) next b pred
                                                        else filter_accum_helper combiner total term (next a) next b pred


--Ex 1.33a
sum_of_squares_of_primes :: Int -> Int -> Int
sum_of_squares_of_primes a b = filter_accum (+) 0 square a (+1) b is_prime


--Ex 1.33b
integers_relatively_prime_product :: Int -> Int
integers_relatively_prime_product n = filter_accum (*) 1 (+0) 1 (+1) n (relative_prime n)


--Ex 1.35
fixed_point :: (a -> a) -> a -> (a -> a -> Bool) -> a 
fixed_point f x close_enough = if (close_enough x next) then next
                               else fixed_point f next close_enough

                               where next = f x

close_enough :: Double -> Double -> Bool
close_enough guess next_guess = let accuracy_limit = 0.00001 in abs (guess - next_guess) < accuracy_limit

golden_ratio :: Double
golden_ratio = fixed_point (\x -> 1.0 + (1.0 / x)) 1 close_enough


--Ex 1.37
cont_frac :: Double -> Double -> Int -> Double
cont_frac n d k = if (k == 1) then n / d
                  else n / (d + cont_frac n d (k - 1))

golden_ratio_frac :: Double
golden_ratio_frac = let repetitions = 10000 in 1 / cont_frac 1 1 repetitions


--Ex 1.37b
cont_frac_iter :: Double -> Double -> Int -> Double
cont_frac_iter n d k = frac_helper n d k 0.0

frac_helper :: Double -> Double -> Int -> Double -> Double
frac_helper n d k total = if (k == 1) then total
                          else frac_helper n d (k - 1) (n / (total + d))


--Ex 1.38, modified to deal with list inputs rather than fixed values
cont_frac_list :: [Double] -> [Double] -> Int -> Double
cont_frac_list (ni:n) (di:d) k = if (k == 1) then ni / di
                                 else ni / (di + cont_frac_list n d (k - 1))

generate_sequence :: Int -> Double -> [Double]
generate_sequence counter add = if (counter `mod` 3 == 0) then add:(generate_sequence (counter + 1) (add + 2))
                                else 1:(generate_sequence (counter + 1) add)

di :: [Double]
di = 1:(generate_sequence 0 2)

e_sicp :: Double
e_sicp = let repetitions = 10000 in cont_frac_list (cycle [1]) di repetitions + 2


--Ex 1.39, modified to be more generic, have to pass in a function to decide how to combine fractions
cont_frac_list_generic :: [Double] -> [Double] -> (Double -> Double -> Double) -> Int -> Double
cont_frac_list_generic (ni:n) (di:d) f k = if (k == 1) then ni / di
                                           else ni / (f di (cont_frac_list_generic n d f (k - 1)))

tan_approx :: Double -> Double
tan_approx x = cont_frac_list_generic top_sequence [1, 3..] (-) 10000

               where top_sequence = x:(cycle $ [x ^ 2])


--Ex 1.40
cubic :: Double -> Double -> Double -> (Double -> Double)
cubic a b c = (\x -> x ^ 3 + (a * x ^ 2) + (b * x) + c)


--Ex 1.41, could also just use Haskell's operator i.e double_sicp f = f . f
double_sicp :: (a -> a) -> (a -> a)
double_sicp f = (\n -> f $ f n) 


--Ex 1.42, again could also just use Haskell's operator i.e compose f g = f . g
compose :: (a -> c) -> (b -> a) -> (b -> c)
compose f g = (\n -> f $ g n)


--Ex 1.43
repeated :: (a -> a) -> Int -> (a -> a)
repeated f n = repeated_helper f f n

repeated_helper :: (a -> a) -> (a -> a) -> Int -> (a -> a)
repeated_helper f f' n | n == 1    = f'
                       | otherwise = repeated_helper f (f . f') (n - 1)


--Ex 1.44
smooth :: (Double -> Double) -> (Double -> Double)
smooth f = (\x -> let dx = 0.05 in (f(x - dx) + f x + f (x + dx)) / 3)

n_smooth ::  Int -> (Double -> Double) -> (Double -> Double)
n_smooth = repeated smooth


--Ex 1.45
nth_root :: Int -> Double -> Double
nth_root n x = fixed_point (mult_dampen root_function) x good_enough

               where mult_dampen = repeated average_damp damps_needed
                     root_function = (\y -> x / (y ^ (n - 1)))
                     damps_needed = floor_log 2 n

average_damp :: (Double -> Double) -> (Double -> Double)
average_damp f = (\n -> (f n + n) / 2)


--Ex 1.46
iterative_improve :: (a -> Bool) -> (a -> a) -> (a -> a)
iterative_improve good_enough improve = (\guess -> if (good_enough guess) then guess else iterative_improve good_enough improve (improve guess)) 

sqrt_iter_improve :: Double -> Double
sqrt_iter_improve x = (iterative_improve good_enough_func improve_func) 1

                      where good_enough_func = (\guess -> let threshold = 0.00001 in abs(x - guess ^ 2) < threshold)
                            improve_func = (\guess -> average guess (x / guess))

fixed_point_improve :: (a -> a) -> a -> (a -> Bool) -> a 
fixed_point_improve f x good_enough = (iterative_improve good_enough f) x


-- Some helper functions
relative_prime :: Int -> Int -> Bool
relative_prime n a = gcd n a == 1

double :: Int -> Int
double n = shift n 1

half :: Int -> Int
half n = shift n (-1)

average :: Double -> Double -> Double
average x y = (x + y) / 2.0

floor_sqrt :: Int -> Int
floor_sqrt = floor . sqrt . fromIntegral

square :: (Num a) => a -> a
square x = x * x

floor_log :: Int -> Int -> Int
floor_log n val = floor $ logBase (fromIntegral n) (fromIntegral val)

inc :: Int -> Int
inc x = x + 1









 



