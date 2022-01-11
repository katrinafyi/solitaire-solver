module Solitaire where

import Data.Char (toLower)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe, fromJust, isJust, isNothing, catMaybes)
import Data.List (transpose, partition, foldl1')
import Data.Function (on)
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)), nonEmpty)
import Control.Monad (guard)
import Data.Foldable (Foldable(toList))

data N = N6 | N7 | N8 | N9 | N10
    deriving (Eq, Show, Ord, Enum, Bounded)

data Colour = R | B deriving (Show, Eq)
data Suit = C | D | H | S deriving (Show, Eq)

data Card = Num Colour N | Face Suit deriving (Show, Eq)

data Board = Board { boardStacks :: [[Card]], boardBlank :: Maybe Card }
    deriving (Show, Eq)

data Move = Move (Maybe Int) (Maybe Int) (NonEmpty Card) deriving (Show, Eq)

moveCards :: Move -> NonEmpty Card
moveCards (Move _ _ c) = c

flipColour :: Colour -> Colour
flipColour R = B
flipColour B = R

matches :: Card -> Card -> Bool
matches (Num c1 n1) (Num c2 n2)
    | n1 < maxBound = flipColour c1 == c2 && succ n1 == n2
matches (Face s1) (Face s2) = s1 == s2
matches _ _ = False

allMatches :: NonEmpty Card -> Bool
allMatches cs = and $ zipWith matches (toList cs) (N.tail cs)

isNum :: Card -> Bool
isNum (Num _ _) = True
isNum _ = False

isFace :: Card -> Bool
isFace = not . isNum

parseSuit :: Char -> Maybe Suit
parseSuit 'c' = Just C
parseSuit 'd' = Just D
parseSuit 'h' = Just H
parseSuit 's' = Just S
parseSuit _ = Nothing

parseColour :: Char -> Maybe Colour
parseColour 'r' = Just R
parseColour 'b' = Just B
parseColour _ = Nothing

parseN :: String -> Maybe N
parseN "6" = Just N6
parseN "7" = Just N7
parseN "8" = Just N8
parseN "9" = Just N9
parseN "10" = Just N10
parseN _ = Nothing

parseCard :: String -> Maybe Card
parseCard (x:xs) = (Face <$> parseSuit x) <|> (Num <$> parseColour x <*> parseN xs)
parseCard _ = Nothing

parseBoard :: String -> Board
parseBoard raw = Board (catMaybes . reverse <$> transpose cards) Nothing
    where cards = fmap (fmap parseCard . words) $ lines $ fmap toLower raw

movable :: NonEmpty Card -> NonEmpty Card
movable (x:|xs) = x :| fmap snd (takeWhile (uncurry matches) rest)
    where rest = zip (x:xs) xs

-- liftMove :: ((Int, Maybe Card) -> (Int, Maybe Card) -> a) -> Move -> a
-- liftMove f (Move a b) = f a b

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

nonEmptyStacks :: [[a]] -> [(Int, NonEmpty a)]
nonEmptyStacks stacks = fmap fromJust <$> filter (isJust . snd) s'
    where s' = withIndex $ nonEmpty <$> stacks

emptyStacks :: [[a]] -> [Int]
emptyStacks stacks = fst <$> filter (isNothing . snd) s'
    where s' = withIndex $ nonEmpty <$> stacks

-- moves a non-empty stack to another non-empty stack.
stackMoves :: Board -> [Move]
stackMoves (Board stacks _) = do
    (i, s1) <- nonEmptyStacks stacks
    (j, s2) <- nonEmptyStacks stacks
    guard $ i /= j
    let m = movable s1
    guard $ matches (N.last m) (N.head s2)

    pure $ Move (Just i) (Just j) m

liftToMove :: (Card -> a) -> Move -> a
liftToMove f = f . N.head . moveCards

-- moves from a non-empty stack to the blank space.
toBlankMoves :: Board -> [Move]
toBlankMoves (Board stacks (Just _)) = []
toBlankMoves (Board stacks Nothing) = do
    (i, s) <- nonEmptyStacks stacks
    pure $ Move (Just i) Nothing (pure $ N.head s)

-- moves from the blank space to any other stack
fromBlankMoves :: Board -> [Move]
fromBlankMoves (Board stacks Nothing) = []
fromBlankMoves (Board stacks (Just x)) = do
    (i, s) <- withIndex stacks
    guard $ maybe True (matches x . N.head) (nonEmpty s)
    pure $ Move Nothing (Just i) (pure x)

allMoves :: Board -> [Move]
allMoves = do
    fromBlanks <- fromBlankMoves
    (numMoves, faceMoves) <- partition (liftToMove isNum) <$> stackMoves
    toBlanks <- toBlankMoves

    pure $ fromBlanks ++ numMoves ++ faceMoves ++ toBlanks

doMove :: Move -> Board -> Board
doMove move (Board stacks blank) = Board stacks' blank'
    where
        stacks' = doMoveOnStack move <$> withIndex stacks
        blank' = doMoveOnBlank move blank

doMoveOnStack :: Move -> (Int, [Card]) -> [Card]
doMoveOnStack (Move i j cs) (n, s)
    | Just n == i = drop (length cs) s
    | Just n == j = toList (N.reverse cs) ++ s
    | otherwise = s

doMoveOnBlank :: Move -> Maybe Card -> Maybe Card
doMoveOnBlank (Move Nothing _ _) _ = Nothing
doMoveOnBlank (Move _ Nothing cs) _ = Just (N.head cs)
doMoveOnBlank Move {} x = x


isSolved :: Board -> Bool
isSolved (Board stacks blank) = isNothing blank
        && length ss == 8 && length nums == 4 && length faces == 4
        && all allMatches ss
    where
        ss = snd <$> nonEmptyStacks stacks
        (nums, faces) = partition (isNum . N.head) ss

b :: Board
b = Board {boardStacks = [[Num R N6,Num B N7,Num R N8,Face H],[Num B N10,Num R N8,Face S,Num B N6],[Num B N6,Face D,Face D,Face D],[Face H,Face C,Face C,Face C],[Num R N9,Num R N7,Num R N9,Face S],[Num R N10,Num B N9,Face H,Num R N7],[Face C,Face H,Num B N7,Num B N10],[Face S,Num B N8,Num R N6,Num R N10],[Num B N8,Num B N9,Face D,Face S]], boardBlank = Nothing}

showCard :: Card -> String
showCard (Num c n) = show c ++ show n ++ " "
showCard (Face s) = show s ++ "  "

printBoard :: Board -> IO ()
printBoard (Board stacks blank) = do
    putStr "Blank: "
    print blank
    putStrLn "Board:"
    let rows = transpose stacks
    mapM_ putStrLn $ reverse $ concatMap ((++ "  \t") . showCard) <$> transpose stacks

test :: IO ()
test = readFile "t1.txt" >>= main'

test2 :: IO ()
test2 = readFile "t2.txt" >>= main'

main' :: String -> IO ()
main' raw = do
    let board = parseBoard raw
    print board

    print $ allMoves board

    print $ isSolved board

main :: IO ()
main = putStrLn "main"
