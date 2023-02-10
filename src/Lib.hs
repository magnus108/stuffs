module Lib where
    
import Polysemy
import Polysemy.Output
import Control.Monad
import Data.List.Split
import Data.Function

data Row m a where
  Row :: m () -> Row m ()

makeSem ''Row

data Col m a where
  Col :: String -> Col m ()

makeSem ''Col

data Table  m a where
  Table :: m () -> Table m ()

makeSem ''Table


data Bob = Bob String | Sep | Sep2
    deriving (Show, Eq)

data RealTable m a where
  Rbob :: String -> RealTable m ()
  Rsep :: RealTable m ()
  Rsep2 :: RealTable m ()

makeSem ''RealTable

mkRealTable :: (Member RealTable r) => Sem r ()
mkRealTable = do
    rsep2
    rsep
    rbob "a"
    rbob "b"
    rbob "c"
    rsep
    rbob "a1"
    rbob "b1"
    rbob "c1"

data RealTable2 m a where
  Rbob2 :: [Bob] -> RealTable2 m ()

makeSem ''RealTable2

mkRealTable2 :: (Member RealTable2 r) => Sem r ()
mkRealTable2 = rbob2 [Sep2,Sep,Bob "a",Bob "b", Bob "c", Sep, Bob "a1", Bob "b1", Bob "c1"]


mkTable :: (Member Table r, Member Row r, Member Col r) => [[String]] -> Sem r ()
mkTable rows = table $ forM_ rows (\cols -> row $ forM_ cols col)

mkRow :: (Member Row r, Member Col r) => [String] -> Sem r ()
mkRow cols = row $ forM_ cols col

colToOutput :: Member (Output Bob) r => Sem (Col ': r) a -> Sem r a
colToOutput = interpret (\(Col x) -> output (Bob x))

rowToOutput :: Member (Output Bob) r => Sem (Row ': r) a -> Sem r a
rowToOutput = interpretH (\(Row x) -> do
            m' <- rowToOutput <$> runT x
            raise $ do
                output Sep
                m'
        )

tableToOutput :: Member (Output Bob) r => Sem (Table ': r) a -> Sem r a
tableToOutput = interpretH (\(Table x) -> do
            m' <- tableToOutput <$> runT x
            raise $ do
                output Sep2
                m'
        )

lol :: [Bob] -> Sem r [[String]]
lol = pure . foldl lol2 []
    where
        lol2 _ Sep2 = []
        lol2 yss Sep = []:yss
        lol2 (ys:yss) (Bob x) = (x:ys):yss



someFunc :: IO ()
someFunc = do
    let xs = mkRow ["a","b","c"]
                    & colToOutput
                    & rowToOutput
                    & runOutputList @Bob
                    & fmap fst
                    & (>>= lol)
                    & run

    let ys = mkTable [["a","b","c"], ["a1","b1","c1"]]
                    & colToOutput
                    & rowToOutput
                    & tableToOutput
                    & fmap fst . runOutputList @Bob
                    & (>>= lol)
                    & run

    putStrLn "someFunc"
    putStrLn (show ys)
