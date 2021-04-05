module Main where

import Data.Function ( (&) )
import Generator ( generate )
import Grammar
import GrammarLexer
import GrammarParser
import Utils


{-

Input grammar example:
{
module Parser (parse) where
}
plus = "\+"         { const PlusT }     { PlusT }
minus = "-"         { const MinusT }    { MinusT }
star = "\*"         { const StarT }     { StarT }
slash = "\\"        { const SlashT }    { SlashT }
op = "\("           { const OpParT }    { OpParT }
cl = "\)"           { const ClParT }    { ClParT }
n = "[0-9]+"        { NumT . read }     { NumT $$ }
ws = "[[:space:]]+" ;
%%
E -> T E'           { $2 $1 }
E' -> plus T E'     { $3 . flip Add $2 }
E' -> minus T E'    { $3 . flip Sub $2 }
E' ->               { id }
T -> F T'           { $2 $1 }
T' -> star F T'     { $3 . flip Mul $2 }
T' -> slash F T'    { $3 . flip Div $2 }
T' ->               { id }
F -> n              { Num $1 }
F -> op E cl        { $2 }
F -> minus F        { Neg $2 }
{
data Token  = OpParT
            | ClParT
            | PlusT
            | MinusT
            | StarT
            | SlashT
            | NumT Int
            deriving Eq

data Expression
  = Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Neg Expression
  | Num Int
  deriving (Show, Eq)
}

-}
main :: IO ()
main = do
  fileName <- getLine
  genParser fileName


genParser :: FilePath -> IO ()
genParser filePath =
  removeExtension ".grammar" filePath
    & maybe (putStrLn $ filePath ++ " is not '.grammar' file") writeWithFileBase
  where
    writeWithFileBase fileBase = do
      input <- readFile filePath
      let (initData, tokens, rules, finalData) = parseGrammar . alexScanTokens $ input
      let genCode = generate initData tokens rules finalData
      genCode & either (putStrLn . ("error: " ++)) (writeWithCode fileBase)
    writeWithCode fileBase genCode = do
      common <- readFile "assets/Common.hs"
      let dir = extractDir filePath
      writeFile (dir ++ "Common.hs") common
      writeFile (fileBase ++ ".hs") genCode
