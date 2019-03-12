
-- foo around wth input .. two numbers per line .. print it back after C-d

import Control.Monad

main = forever $ do
    l <- getLine
    putStrLn l
    
