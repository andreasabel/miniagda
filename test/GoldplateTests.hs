import System.Exit     ( exitWith  )
import System.Process  ( system    )

main :: IO ()
main = exitWith =<< system ("goldplate test")
