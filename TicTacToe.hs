
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Ord

import Control.Arrow
import Control.Monad
import Control.DeepSeq

import Control.Parallel.Strategies

import System.IO

data Field = Empty | FilledBy Player deriving (Show, Read, Eq)

type Board = [Field]

type Position = Int

data Player = O | X deriving (Show, Read, Eq, Ord)

type Move = (Position, Player)

type State = (Player, Board)

type Chance = Int

-- (current players chances, other players chances)
type Score = (Chance, Chance)

data Path = Path State Score (M.Map Move Path) | End Score Player deriving (Show, Read)


instance NFData Path where
    rnf end@(End _ _) = end `seq` ()
    rnf path@(Path _ _ followups) = M.map rnf followups `seq` ()

instance NFData Player where
    rnf player = player `seq` ()


main = putStrLn "Hang on, calculating moves..." >> play O (paths (X, replicate 9 Empty))


play human path =
    case path of
        (Path state@(player, board) score followups)
            | M.null followups -> putStrLn "Draw."
            | otherwise        -> turn human path >>= play human

        (End _ winner)
            | winner == human  -> putStrLn "You win."
            | otherwise        -> putStrLn "AI wins."


turn human path@(Path _ _ followups) =
    fmap (fromJust . flip M.lookup followups) (getMove human path)


getMove human path@(Path (player, _) _ _)
    | human == player = getHumanMove human path
    | otherwise = return $ bestMove path


getHumanMove human path@(Path (_, board) _ followups) = do
    showBoard board >> putStr prompt >> hFlush stdout

    input <- getLine

    case reads input of
        [(n, "")]
            | M.member (n, human) followups -> return (n, human)
            | otherwise -> putStrLn "Invalid move, try again." >> again
        _ -> putStrLn "Invalid input, try again." >> again

    where
        again = getHumanMove human path
        prompt = unwords (map (show . fst) $ M.keys followups) ++ "> "


bestMove (Path _ _ followups) =
    fst $ head $ sortBy (comparing (score . snd)) $ M.toList followups
    where
        score (Path _ (x, o) _) = fromIntegral x / fromIntegral o
        score (End _ player) = - (1 / 0)


availableMoves (p, board) = map (flip (,) p) $ findIndices (== Empty) board


applyMove b (pos, p) = xs ++ FilledBy p : xs' where (xs, _ : xs') = splitAt pos b

paths state@(player, board) =
    case winner board of
        Nothing -> path
        Just winningPlayer -> End (0, 1) winningPlayer
    where
        followups = M.fromList
            $ parMap rdeepseq (id &&& paths . (,) (nextP player) . applyMove board)
            $ availableMoves (player, board)

        path = Path state (sumScore (map pathScore $ M.elems followups)) followups

        pathScore (End score _) = score
        pathScore (Path _ score _) = score

        addScore (x, o) (o', x') = (x + x', o + o')

        sumScore = foldl addScore (0, 0)


nextP O = X
nextP X = O


winner board = listToMaybe uniforms
    where
        lines = map (map (board !!)) [
                [0 .. 2], [3 .. 5], [6 .. 8],
                [0, 3, 6], [1, 4, 7], [2, 5, 8],
                [0, 4, 8], [2, 4, 6]
            ]

        uniforms = map player $ filter isPlayer $ map head $ filter uniform lines

        isPlayer (FilledBy _) = True
        isPlayer _ = False

        player (FilledBy p) = p

        uniform [] = True
        uniform (x:xs) = all (== x) xs


showBoard board =
    forM_ [[0..2], [3..5], [6..8]] $ \ row -> do
        forM_ row $ \ i -> putStr [showField (board !! i), ' ']
        putStr "\n"


showField (FilledBy X) = 'X'
showField (FilledBy O) = 'O'
showField Empty        = '_'
