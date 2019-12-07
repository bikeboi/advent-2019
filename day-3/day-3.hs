module Main where

main :: IO ()
main = input >>= print . uncurry solution2


test1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
test2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
test3 = "R8,U5,L5,D3\nU7,R6,D4,L4"

(tw1,tw2) = let (w1,w2) = parseInput $ test1
          in (buildWire w1, buildWire w2)

data Dir = R | U | L | D deriving (Eq, Show)
data Step = Step Dir Int deriving (Eq, Show)

data Line = HLine Int Int Int -- y x x'
          | VLine Int Int Int -- x y y'
          deriving (Eq, Show)

type Point = (Int,Int)
type Moves = [Step]
type Path = [Line]

input :: IO (Moves,Moves)
input = parseInput <$> readFile "input.txt"

parseInput :: String -> (Moves,Moves)
parseInput s = let [w1,w2] = (fmap . fmap) parse . map words . lines . map commaSpace $ s
               in (w1,w2)
  where commaSpace c = if c == ',' then ' ' else c
        parse (dir:num) = Step (parseDir dir) (read num)
        parseDir 'R' = R
        parseDir 'U' = U
        parseDir 'L' = L
        parseDir 'D' = D

-- Solution
solution1 ms ns = closestManhattan $ intersections (buildWire ms) (buildWire ns)

solution2 :: Moves -> Moves -> Int
solution2 ms ns = let (w1,w2) = (buildWire ms, buildWire ns)
                      its = intersections w1 w2
                      total = (+) <$> stepsTo w1 <*> stepsTo w2
                  in minimum . map total $ its

testPath = [ HLine 0 0 8, VLine 8 0 5, HLine 5 8 0, VLine 0 5 10 ]

stepsTo :: Path -> Point -> Int
stepsTo [] _ = -1
stepsTo (line:rest) point = if point `liesOn` line
                            then stepsTo' line point
                            else let to = stepsTo rest point
                                 in if to == -1 then -1 else dist line + to
  where stepsTo' :: Line -> Point -> Int
        stepsTo' (HLine _ x x') (px,_) = abs (px - x)
        stepsTo' (VLine _ y y') (_,py) = abs (py - y)

walk :: Path -> [Point]
walk = fix . concat . map buildLine
  where fix [] = []
        fix (x:xs) = if x `elem` xs
                     then x : (tail . fix . dropWhile (/= x)) xs
                     else x : fix xs

closestManhattan :: [Point] -> Int
closestManhattan = minimum . map (manhattan central)

intersections :: Path -> Path -> [Point]
intersections xs ys = filter (/= central)
                      . map (uncurry intersection)
                      . filter (uncurry intersects)
                      $ (,) <$> xs <*> ys

intersection :: Line -> Line -> Point
intersection v@(VLine _ _ _) h@(HLine _ _ _) = intersection h v
intersection h@(HLine _ _ _) v@(VLine _ _ _) = let hpoints = buildLine h -- Tail ignores central point
                                                   vpoints = buildLine v
                                               in head
                                                  . map fst
                                                  . filter (uncurry (==))
                                                  $ (,) <$> hpoints <*> vpoints

intersects :: Line -> Line -> Bool
intersects (HLine hy x x') (VLine vx y y') = between vx x x' && between hy y y'
intersects v@(VLine _ _ _) h@(HLine _ _ _) = intersects h v
intersects _ _ = False

buildWire :: Moves -> Path
buildWire = snd . foldl f (central, [])
  where f (point, path) s = let (next,line) = hop point s
                            in (next, path ++ [line] )

hop :: Point -> Step -> (Point, Line)
hop (x,y) (Step dir n) = case dir of
                            R -> ((x+n,y), HLine y x (x+n))
                            L -> ((x-n,y), HLine y x (x-n))
                            U -> ((x,y+n), VLine x y (y+n))
                            D -> ((x,y-n), VLine x y (y-n))

-- Utility
buildLine :: Line -> [Point]
buildLine (HLine y x x') = flip (,) y <$> interpolate x x'
buildLine (VLine x y y') = (,) x <$> interpolate y y'

dist :: Line -> Int
dist (HLine _ x x') = abs (x - x')
dist (VLine _ y y') = abs (y - y')

liesOn :: Point -> Line -> Bool
liesOn (px,py) (HLine y x x') = y == py && between px x x'
liesOn (px,py) (VLine x y y') = x == px && between py y y'

interpolate :: Int -> Int -> [Int]
interpolate x y = let st = if y < x then -1 else 1
                  in [x, x + st .. y]

manhattan :: Point -> Point -> Int
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

central :: Point
central = (0,0)

between :: Int -> Int -> Int -> Bool
between a x y = a >= x && a <= y || a <= x && a >= y
