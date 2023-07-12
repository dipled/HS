import Data.List

qs :: Ord a => [a] -> [a]

-- Note que o pivo utilizado foi justamente o primeiro elemento da lista.
qs [] = []
qs [a] = [a]
qs (x:xs) = (qs (filter (< x) xs)) ++ x:(filter (== x) xs) ++ (qs(filter (> x) xs))


ms :: Ord a => [a] -> [a]

ms [] = []
ms [x] = [x]

ms(xs) = msAux (ms fstHalf) (ms sndHalf)
    where 
        (fstHalf, sndHalf) = splitAt (div (length xs) 2) xs


msAux :: Ord a => [a] -> [a] -> [a]

msAux xs [] = xs
msAux [] ys = ys
msAux (x:xs) (y:ys)
    |x < y = x:(msAux xs (y:ys))
    |otherwise = y:(msAux (x:xs) ys)


main :: IO()
main = 
    do
        putStrLn(show(ms [1000000,999999..0]))
