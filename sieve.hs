sieve::[Integer]->[Integer]
sieve(n:xs)= n:sieve(filter (\m->(rem m n)/=0)xs)

primes' :: [Integer]
primes' = sieve [2..]