import System.IO
import Data.Char(toUpper)

main = do
       texto  <- openFile "texto" ReadMode
       salida <- openFile "salida" WriteMode
       texto `htoUpper` salida
       hClose texto
       hClose salida

htoUpper :: Handle -> Handle -> IO ()
htoUpper input output = 
       do
       eof <- hIsEOF input
       if eof 
          then return ()
          else 
           -- do notation
           {- do
           line <- hGetLine input
           hPutStrLn output $ toUpper <$> line
           htoUpper input output -}
           -- without do notation
           hGetLine input >>= (\ line ->
              (hPutStrLn output $ toUpper <$> line) >>
                htoUpper input output
           )    
