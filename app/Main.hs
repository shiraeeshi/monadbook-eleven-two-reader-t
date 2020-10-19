module Main where

import Lib

oldmain :: IO ()
oldmain = do
  let env = Enviro 100 2 3
      rdr = xyz
      --rdr2 = fmap (flip (-) 88) rdr
      --rdr2 = (flip (-) 89) <$> rdr
      rdr2 = (+) <$> pure (- 90) <*> rdr
      multiplied = runReader rdr2 env
  putStrLn $ "result: " ++ (show multiplied)

main :: IO ()
main = do
  let env = Enviro 100 2 3
      rdr = listedXMultY
      result = runReaderT rdr env
  putStrLn $ "result: " ++ (show result)
