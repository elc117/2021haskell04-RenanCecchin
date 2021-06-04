-- Prática 04 de Haskell
-- Nome: Renan de Siqueira Cecchin

faixaIdoso :: Int -> String
faixaIdoso x
    | x >= 60 && x <= 64 = "IDO64"
    | x >= 65 && x <= 69 = "IDO69"
    | x >= 70 && x <= 74 = "IDO74"
    | x >= 75 && x <= 79 = "IDO79"
    | x >= 80 = "IDO80"
    | otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos tuples = [ (x, y, faixaIdoso y)| (x,y) <- tuples]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' tuples = map (\(x,y) -> (x,y, faixaIdoso y)) tuples

strColor :: (Int,Int,Int) -> String
strColor (r, g, b) = "rgb(" ++ show(r) ++ "," ++ show(g) ++ "," ++ show(b) ++ ")"

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (x, y) r = reverse [(x, y, n) | x <- [x + (r^2 * (n-1)), x + (2^2 * (n-2)) .. x]]

genReds :: Int -> [(Int, Int, Int)]
genReds n = map (\(x,y,z) -> if(x * (n `mod`7) > 255) then (x, 0, 0) else (x * (n `mod`7), 0, 0)) [(red, 0, 0) | red <- [50, 60 .. 50 + (10 * n)]]
