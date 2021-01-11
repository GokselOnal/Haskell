divides d n = rem n d == 0

prime0 n | n < 1     = error "i/p not valid"
         | n == 1    = False
         | otherwise = ld n == n

ld n = ldf 2 n

ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

square::Integral a => a->a
square x = x * x
 
mult2::Integer -> Integer
mult2 x = 2 * x

d3 = divides 3