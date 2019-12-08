module Day4 where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe,listToMaybe)

--
sol :: IO ()
sol = do orbs <- lines <$> readFile "./src/input/input-6.txt"
         let space = weighOrbits . buildSpace . foldl readOrbit M.empty $ orbs
         print $ commonParent "YOU" "SAN" space
         print $ minTransfers "YOU" "SAN" space
           where testIn =
                   ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"]

--
data Tree a = Node a [Tree a] | Leaf a deriving (Eq, Show)
data Body a = CM | Orbit a (Body a) deriving (Eq, Show)

type Store = M.Map String [String]
type Space = Tree (String,Int)
type Weight = (String,Int)
type Path = [Weight]


name :: Space -> String
name (Leaf (s,_)) = s
name (Node (s,_) _) = s

-- PART 2
minTransfers :: String -> String -> Space -> Maybe Int
minTransfers s s' sp = do ((_,pw),(_,cw),(_,cw')) <- commonParent s s' sp
                          return $ (cw - pw) + (cw' - pw) - 2

commonParent :: String -> String -> Space -> Maybe (Weight,Weight,Weight)
commonParent s s' sp = do p1 <- pathTo s sp
                          p2 <- pathTo s' sp
                          (,,) <$> overlap p1 p2 <*> revPop p1 <*> revPop p2
  where overlap p p' = listToMaybe . reverse . map fst . takeWhile (uncurry (==)) $ zip p p'
        revPop = listToMaybe . reverse

pathWeight :: Path -> Int
pathWeight = foldl (\z (_,n) -> z + n) 0

pathTo :: String -> Space -> Maybe Path
pathTo s (Leaf (s',n)) = if s == s' then Just [(s',n)] else Nothing
pathTo s (Node (s',n) cs) = if s == s'
                            then Just [(s',n)]
                            else case (concat . mapMaybe (pathTo s)) cs of
                                   [] -> Nothing
                                   ps -> Just $ (s',n):ps
--

-- PART 1
sumOrbits :: Space -> Int
sumOrbits (Leaf (_,n)) = n
sumOrbits (Node (_,n) rest) = n + (sum $ map sumOrbits rest)

weighOrbits :: Tree String -> Space
weighOrbits body = go 0 body
  where go n (Leaf s) = Leaf (s,n)
        go n (Node s xs) = Node (s,n) (map (go $ n + 1) xs)

buildSpace :: Store -> Tree String
buildSpace st = connectChildren "COM"
  where connectChildren s = case M.member s st of
                              False -> Leaf s
                              True -> Node s $ map connectChildren $ st M.! s

readOrbit :: Store -> String -> Store
readOrbit st s = let (center,child) = fmap tail $ span (/= ')') s
                 in M.insertWith (\cs c -> c ++ cs) center [child] st

--
