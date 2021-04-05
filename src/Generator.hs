module Generator
  ( generate
  ) where

import Data.Char
import Data.Function ( (&) )
import Data.List ( delete, find, intercalate, null, uncons )
import Data.Maybe ( fromMaybe, isJust )
import Grammar
import Utils ( collect, maybeToEither, replaceNumberStub, replaceStub )

import qualified Data.Map.Strict as Map

type TokenToIndex = Map.Map String Int

myImports :: [String]
myImports =
  [ "Common"
  , "Text.Regex.TDFA ( (=~~) )"
  , "Control.Exception ( assert )"
  , "Data.Function ( (&) )"
  ]

generate :: String -> [Token] -> [Rule] -> String -> Either String String
generate moduleBlock tokens rules dataTypes = do
  nonTerminals <- collect rules & mapM (uncurry $ generateNonTerminal tokenToIndex)
  let imports = myImports & map ("import " ++) & intercalate "\n"

  return $ unlines
    [ moduleBlock
    , imports
    , dataTypes
    , mainFunction
    , generateTokenizer tokens
    , unlines tokenParsers
    , unlines nonTerminals]

  where
    mainFunction =
      unlines
        [ "parse str = do"
        , "  tokens <- tokenize str"
        , "  runP (parse_" ++ firstNT ++ " <* eof) tokens & maybe"
        , "      (Left \"failed to parse\") (return . fst)"
        ]
    firstNT = head rules & fst

    validTokens = filter (\(Token _ _ maybeCode) -> isJust maybeCode) tokens
    tokenToIndex = Map.fromList (map getTokenName validTokens `zip` [1 ..])
    getTokenName (Token name _ _) = name
    generateTokenParsers (Token name _ (Just (_, constructor))) =
      generateTokenParser (tokenToIndex Map.! name) constructor
    tokenParsers = validTokens & map generateTokenParsers


generateTokenizer :: [Token] -> String
generateTokenizer tokens = helperFunction ++ "\n" ++ tokenizerFunction
  where
    helperFunction =
      unlines
        [ "prefixMatch :: String -> String -> Maybe (String, String)"
        , "prefixMatch str regex = do"
        , "    (left, middle, right) <- str =~~ ('\\\\' : '`' : regex)"
        , "    assert (\"\" == left) $ return (middle, right)"
        ]

    tokenizerFunction =
      unlines
        [ "tokenize [] = return []"
        , tokens & map makeTokenizeLine & intercalate "\n"
        , "tokenize str = Left $ \"Tokenization failure at '\" ++ str ++ \"'\""
        ]

    makeTokenizeLine (Token _ regex maybeCode) =
      "tokenize str | Just (match, rest) <- prefixMatch str " ++
      show regex ++
      " = " ++
      (case maybeCode of
         Nothing        -> ""
         Just (code, _) -> "( (" ++ code ++ ") match :) <$> ") ++
      "tokenize rest"


generateNonTerminal :: TokenToIndex -> String -> [([Element], Code)] -> Either String String
generateNonTerminal tokenToIndex nonTerminals productionsPairs = do
  let epsProduction = find (null . fst) productionsPairs
  let (productions, codes) = unzip productionsPairs
  let lengths = map length productions

  rules' <- sequence $ zipWith makeRule [1 ..] productions
  case uncons rules' of
    Nothing -> Left $ "No productions for " ++ nonTerminals
    Just (rule, rules) ->
      return $
      concat
        [ "parse_" ++ nonTerminals ++ "\n"
        , "    = " ++ rule ++ "\n" ++ unlines (map ("  <|> " ++) rules)
        , "  where\n"
        , unlines $ zipWith3 makeBindLine [1 ..] lengths codes
        ]

  where
    makeRule i = generateRule tokenToIndex ("rule" ++ show i)
    makeBindLine i count code = "    " ++ generateBind ("rule" ++ show i) count code


generateRule :: TokenToIndex -> String -> [Element] -> Either String String
generateRule tokenToIndex ruleName prod = do
  funcs <- mapM genFunc prod
  let args =
        if null prod -- case head prod of (Epsilon)
        then " <$ ok"
        else " <$> " ++ intercalate " <*> " funcs

  return $ ruleName ++ args

  where
    genFunc (T t)
      | Just i <- Map.lookup t tokenToIndex = return $ "parseTok_" ++ show i
      | otherwise = Left "Unkown terminal"
    genFunc (NT nt) = return $ "parse_" ++ nt

generateBind :: String -> Int -> Code -> String
generateBind ruleName count code =
  ruleName ++ " " ++ unwords (map (("v" ++) . show) [1 .. count]) ++ " = " ++ replaceNumberStub "v" code

generateTokenParser :: Int -> String -> String
generateTokenParser tokenIndex constructor
  | Just replaced <- replaceStub "v" constructor = common ++ "token getV\n" ++ makeTokenExtractorBinding replaced
  | otherwise = common ++ "single $ " ++ constructor
  where
    common = "parseTok_" ++ show tokenIndex ++ " = "

makeTokenExtractorBinding :: String -> String
makeTokenExtractorBinding constructor = unlines
  [ "  where"
  , "    getV (" ++ constructor ++ ") = Just v"
  , "    getV _ = Nothing"
  ]
