module Lib2 where
    
import Polysemy
import Polysemy.Output
import Control.Monad
import Data.List.Split
import Data.Function

data Col m a where
  Col :: String -> Col m ()

makeSem ''Col


colToData :: Member (Embed (ColData [])) r => Sem (Col ': r) a -> Sem r a
colToData = interpret (\(Col x) -> embed $ ColData [()])

newtype ColData m a = ColData { unColData :: m a }
    deriving (Functor, Applicative, Monad)


data Row m1 m a where
  Row :: ColData m1 a -> Row m1 m ()

makeSem ''Row


myCol :: Member Col r => Sem r ()
myCol = col "Jeu"


myRow :: Member (Row []) r => ColData [] () -> Sem r ()
myRow x  = row x



rowToOutput :: Sem (Row []': r) a -> Sem r a
rowToOutput = interpret (\(Row x) -> do
            return ()
        )

mainer :: IO ()
mainer = do

    let x = myCol 
                & colToData
                & runM @(ColData [])

    let gg = myRow x
                & rowToOutput


    putStrLn "someFunc"
