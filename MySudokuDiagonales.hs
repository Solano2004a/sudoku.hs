module MySudokuDiagonales where
import Data.List((\\),nub,transpose, maximumBy)
import Data.Ord (comparing)
import Control.Applicative (Alternative((<|>)))


type Grid = Matrix Char
type Matrix a = [Row a] 
type Row a = [a]
type Choices = [Char]
--type Grid = [String]
--type ExtendedGrid = [[String]]


--VALIDAR SUDDOKU INGRESADO
validate::Grid->Bool
validate grid = all isValid (rows grid) &&
                all isValid (cols grid) &&
                all isValid (boxes grid) &&
                isValid (diagonalLeft grid) &&
                isValid (diagonalRight grid)
isValid:: [Char] -> Bool
isValid file = let digits = filter (/= '-') file
        in length digits == length (nub digits)




--POSIBLES SOLUCIONES EN '-'
--posibles valores para celdas vacias
possibleValue::Grid->(Int,Int)->[Char]
possibleValue grid (r,c)  | current /= '-'  = [current]
                          | isInDiagonalLeft grid (r,c) || isInDiagonalRight grid (r,c) = "123456789" \\ nub (rowValues ++ colValues ++ blockValues ++ diagonalValues)
                          | otherwise               = "123456789" \\ nub (rowValues ++ colValues ++ blockValues) --nub devuelve un valor de los valores repetidos
                          where
                            current             = (grid!!r)!!c --accede al valor r de row y luego al valor c de column 
                            rowValues           = grid!!r --muestra los valores de la fila r
                            colValues           = transpose grid!!c --transpone la columna c solamente
                            blockValues         = getBlock grid (r,c)
                            diagonalValues      = getDiagonal grid (r,c)
                            

isInDiagonalLeft::Grid->(Int,Int)->Bool
isInDiagonalLeft grid (x,y) = x == y

isInDiagonalRight::Grid->(Int,Int)->Bool
isInDiagonalRight grid (x,y) = x + y == n-1
  where
    n = length grid --9

--obtener todos los valores de las coordenadas que sumadas den exactamente n 
--obtener todos los valores posibles para valores que se encuentran en diagonales
getDiagonal::Grid ->(Int,Int)->[Char]
getDiagonal grid (r,c)  | isInDiagonalLeft grid (r,c) && isInDiagonalRight  grid (r,c) = nub (getDiagonalLeft grid (r,c) ++ getDiagonalRight grid (r,c))
                        | isInDiagonalLeft grid (r,c)   = getDiagonalLeft grid (r,c)
                        | isInDiagonalRight  grid (r,c) = getDiagonalRight grid (r,c)
                        | otherwise                     = []
  where
    --(r,c) coordenadas valor en la diagonal
    getDiagonalLeft g (r,c)   = [g!!r'!!c' | r' <- [0..8], c' <- [0..8], r' == c']
    getDiagonalRight g (r,c)  = [g!!r'!!c' | r' <- [0..8], c' <- [0..8], r' + c' == 8]


--me indica en que bloque se encuentra mi digito y devuelve el bloque en una lista
getBlock :: Grid -> (Int,Int) -> [Char]
getBlock grid (r,c) = [grid!!r'!!c'| r' <- blockRange r, c' <- blockRange c]
  where
    blockRange x = let start = (x `div` 3) *3 in [start..start+2]
    --x `div` 3 me determina el numero de bloque de 0 a 2, *3 me indica en que valor empieza el bloque, tomo el rango [start..start+2] que es el bloque

possibleValues :: Grid -> Matrix Choices
possibleValues grid = [[possibleValue grid (r,c) | (c, cell) <- zip [0..] row] 
                      | (r, row) <- zip [0..] grid]




--EXPANDIR
--expande las celdas con posibles mas pequenios
--una grilla con valores posibles en lista de grillas con valores posibles
expand ::Matrix Choices-> [Matrix Choices]
expand m =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1,row:rows2) = break (any smallest) m
    (row1,cs:row2)    = break smallest row
    smallest c' = length c' == n
    n = minimum (counts m)
    counts = filter (/= 1) . map length . concat



--VALIDACIONES

--Validar grilla completa, celdas con valores unicos (tamanio ==1) en toda la grilla
complete::Matrix Choices -> Bool
complete = all (all singleton)
  where
    singleton l = length l == 1



--validar celdas unicas sin repetirse en una fila, columna, caja o diagonal
nodups::Eq a =>[a]->Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

single::[a] -> Bool
single [_] = True
single _ = False


--obtener cajas 3x3
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat


consistent::Row Choices -> Bool
consistent = nodups . concat . filter single

rows = id

cols = transpose
boxes:: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group . map group

--obtener la diagonal derecha
diagonalRight::Matrix a -> [a]
diagonalRight g = [g!!r!!c | (r,row) <- zip [0..] g, (c,cell) <- zip [0..] row, r+c == n-1]
  where
    n = length g

--obtener la diagonal izquierda
diagonalLeft::Matrix a -> [a]
diagonalLeft g = [g!!r!!c | (r,row) <- zip [0..] g, (c,cell) <- zip [0..] row, r == c]

diagonals::Matrix a -> Matrix a 
diagonals m = [diagonalLeft m, diagonalRight m]

safe::Matrix Choices -> Bool
safe m = all consistent (rows m) &&
         all consistent (cols m) && 
         all consistent (boxes m) && 
         consistent (diagonalLeft m) &&
         consistent (diagonalRight m)

--SEARCH


search :: Matrix Choices -> [Grid]
search m
  | not (safe m) || any (any null) m = [] 
  | complete m = collapse m           
  | otherwise = case concatMap (search . prune) (expand m) of
      []  -> []   
      x:_ -> [x]


solve = search . possibleValues
--PODAR
prune = pruneBy rows . pruneBy cols . pruneBy boxes
  where
    pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
  where
    singles = concat (filter single xss)

minus::Choices->Choices->Choices
xs `minus` ys = if single xs then xs else xs \\ ys

--muestra todas las posibilidades
cp::[[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

collapse::Matrix [a] -> [Matrix a]
collapse = cp . map cp