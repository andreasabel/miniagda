import System.Environment ( getArgs  )
import System.Exit        ( exitWith  )
import System.Process     ( rawSystem )

main :: IO ()
main = exitWith =<< rawSystem "goldplate" . ("test" :) =<< getArgs
