module Semantic where

import Parser qualified

main = do
    putStrLn "Rodando analisador semantico"
    arquivo <- getLine
    conteudo <- readFile arquivo

