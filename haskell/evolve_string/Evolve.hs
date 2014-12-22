
import Data.Ord
import Data.List
import Data.Random
import Control.Monad
import Control.Applicative
import Text.EditDistance
import System.Random



chars = ['a'..'z'] ++ ['A'..'Z'] ++ " !'"

newChar :: IO Char
newChar = go
    where go = do i <- randomRIO (0, length chars - 1)
                  return $ chars !! i


newtype Solution = Solution { solstr :: String }
    deriving (Show)

type Target = Solution

mutate :: Solution -> IO Solution
mutate (Solution s) = do
  i <- randomRIO (0, length s - 1)
  c <- newChar
  let (l,r) = splitAt i s
      new_s = l ++ c : drop 1 r
  -- putStrLn $ "Mut@" ++ show i ++ ": " ++ s ++ " -> " ++ new_s
  return $ Solution new_s
  

cross :: Solution -> Solution -> IO Solution
cross (Solution s1) (Solution s2)
    | length s1 == length s2 = do i <- randomRIO (0, length s1-1)
                                  let new_s = take i s1 ++ drop i s2
                                  -- putStrLn $ "Cross: " ++ s1 ++ " X " ++ s2 ++ " -> " ++ new_s
                                  return $ Solution new_s
    | otherwise              = fail $ "Size mismatch: '" ++ s1 ++ "', '" ++ s2 ++ "'"


fitness :: Target -> Solution -> Double
fitness (Solution s) (Solution t) = fromIntegral $ levenshteinDistance defaultEditCosts s t

randomSolution :: Int -> IO Solution
randomSolution len = Solution <$> mapM (const newChar) [1..len] 

initialize :: Int -> Int -> IO [Solution]
initialize popsize strsize
                    | odd popsize = initialize (popsize +1) strsize
                    | otherwise   = mapM (const (randomSolution strsize)) [1..popsize]


step :: Target -> [Solution] -> IO [Solution]
step target pop = do
  pop' <- sample $ shuffle pop
  let len = length pop
      mid = length pop `div` 2
      (l, r) = (take mid pop', drop mid pop')
  children <- forM (zip l r) $ \pair -> uncurry cross pair >>= mutate
  let newpop = take len $ sortBy (comparing (fitness target)) $ children ++ pop
  return newpop


runGA :: Int -> Target -> [Solution] -> IO Solution
runGA gens target pop = go 0 pop
    where go gen pop = let fit = minimum $ map (fitness target) pop
                       in do putStrLn $ "[" ++ show gen ++ "]: " ++ show fit ++ " @ " ++ solstr (head pop)
                             if or [fit < 1, gen >= gens ]
                                then return $ head pop
                                else step target pop >>= go (gen+1)



data Parameters = Parms { popSize :: Int
                        , maxGens :: Int
                        , seed    :: Int }

defParms = Parms { popSize = 50
                 , maxGens = 9000
                 , seed    = 0 }

run :: String -> IO Solution
run target = let prms = defParms
             in do
               setStdGen $ mkStdGen (seed prms)
               initialize (popSize prms) (length target) >>= runGA (maxGens prms) (Solution target)
