module Utils where

import System.IO (stdout)
import System.Console.ANSI
import Control.Monad


testProg :: String -> IO Bool -> IO ()
testProg name prog = do
  putStrLn ("| Testing " ++ name)

  check <- prog
  if check then
    putCol (Dull, Green) "= Test successful!\n"
  else
    putCol (Dull, Red) "= Test failed!\n"


putCol :: (ColorIntensity, Color) -> String -> IO ()
putCol col str = do
  supported <- hNowSupportsANSI stdout
  if supported then do
    setSGR [uncurry (SetColor Foreground) $ col]
    putStrLn str
    setSGR [Reset]
  else
    putStrLn str 
