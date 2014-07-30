module Main where

import Data.List
import System.Environment
import qualified System.IO as IO
import System.Exit

dataDeclaration
  :: Int
  -- ^ Number of type variables. Must be at least 0.
  -> String
dataDeclaration i
  | i == 0 = dataDeclaration0
  | otherwise = dataDeclarationN i

dataDeclaration0 :: String
dataDeclaration0 = unlines
  [ "data S0 deriving Typeable"
  , ""
  , "instance Eq S0 where _ == _ = undefined"
  , "instance Ord S0 where compare _ _ = undefined"
  , "instance Read S0 where readsPrec _ = undefined"
  , "instance Show S0 where show _ = undefined"
  , "instance Exception S0"
  , ""
  ]


dataDeclarationN
  :: Int
  -- ^ Number of type variables.  Must be at least 1.
  -> String
dataDeclarationN i =
  "data " ++ sType i ++ " = "
  ++ constructors
  ++ " deriving (Eq, Ord, Read, Show"
  ++ (if i <= 7 then ", Typeable)" else ")")
  ++ "\n"
  ++ exceptionInstance i
  where
    constructors = list " | " . map mkConst
      $ ls
    mkConst l = 'S' : show i ++ l : ' ' : l : []
    ls = letters i

exceptionInstance :: Int -> String
exceptionInstance i
  | i > 7 = "\n"
  | otherwise = "instance " ++ constraint
    ++ "Exception (" ++ sType i ++ ")\n\n"
  where
    constraint
      | i == 0 = ""
      | otherwise = "(" ++ list ", " ls ++ ") => "
      where
        ls = map constrain . letters $ i
        constrain c = "Typeable " ++ s ++ ", Show " ++ s
          where s = [c]

letter :: Int -> Char
letter i = toEnum (97 + i)

sType :: Int -> String
sType i = 'S' : show i ++ types
  where
    types | i == 0 = ""
          | otherwise = " " ++ intersperse ' ' ls
    ls = letters i

letters :: Int -> [Char]
letters i = map letter [0..(i - 1)]

list :: [a] -> [[a]] -> [a]
list s = concat . intersperse s

partitionDef :: Int -> String
partitionDef i
  | i == 0 = partitionDef0
  | otherwise = partitionDefN i

partitionDef0 :: String
partitionDef0 = unlines
  [ "partitionS0 :: [S0] -> ()"
  , "partitionS0 _ = ()"
  , ""
  ]

partitionDefN :: Int -> String
partitionDefN i = sig ++ decl
  where
    sig = fName ++ " :: ["
      ++ sType i ++ "] -> " ++ tuple ++ "\n"
    tuple = "(" ++ (list ", " . map lsSig $ ls)
      ++ ")"
    ls = letters i
    lsSig l = '[' : l : ']' : []
    fName = "partitionS" ++ show i

    decl = fName ++ " = foldr fn " ++ tup ++ "\n" ++ whr
      where
        tup = "(" ++ (list ", " . replicate i $ "[]")
          ++ ")"
    whr = "  where\n" ++ cse
    cse = "    fn it " ++ tup ++ " = case it of\n" ++ cases ++ "\n"
      where
        tup = "("
          ++ ( list ", " . map (\l -> l:'s':[])
                $ ls)
          ++ ")"
        cases = concatMap mkCase ls
        mkCase l = "      S" ++ show i ++ l : ' ' : l
          : " -> ("
          ++ (list ", " . map mkLetter $ ls)
          ++ ")\n"
          where
            mkLetter ltr
              | ltr == l = l : ':' : l : 's' : []
              | otherwise = ltr : 's' : []

caseDef :: Int -> String
caseDef i
  | i == 0 = caseDef0
  | otherwise = caseDefN i

caseDef0 :: String
caseDef0 = unlines
  [ "caseS0 :: S0 -> z"
  , "caseS0 = undefined"
  , ""
  ]

caseDefN :: Int -> String
caseDefN i = sig ++ decl
  where
    sig = fName ++ " :: " ++ fns
      ++ " -> " ++ sType i ++ " -> z\n"
    fns = list " -> " . map mkFn $ ls
    ls = letters i
    mkFn l = '(' : l : " -> " ++ "z)"
    fName = "caseS" ++ show i

    decl = fName ++ " " ++ (list " " hofNames) ++ " " ++ sName
      ++ " = case " ++ sName ++ " of\n" ++ cases ++ "\n"
    sName = 's' : show i
    hofNames = map (\s -> 'f':s:[]) ls
    cases = concatMap mkCase ls
    mkCase l = "  S" ++ show i ++ l : ' ' : l
      : " -> " ++ ('f' : l : ' ' : l : "\n")

mapDef :: Int -> String
mapDef i
  | i == 0 = mapDef0
  | otherwise = mapDefN i

mapDef0 :: String
mapDef0 = unlines
  [ "mapS0 :: S0 -> S0"
  , "mapS0 = id"
  , ""
  ]

mapDefN :: Int -> String
mapDefN i = sig ++ decl
  where
    fName = "mapS" ++ show i
    sig = fName ++ " :: " ++ list " -> "
      (fns ++ sType i : resType : []) ++ "\n"
    resType = 'S' : show i ++ (' ' : list " " ls')
    ls = letters i
    ls' = map (\l -> l : '1' : []) ls
    fns = map mkFn ls
    mkFn l = '(' : l : " -> " ++ l : "1)"

    decl = fName ++ " " ++ list " " (map (:[]) ls) ++ " = "
      ++ "caseS" ++ show i ++ " " ++ (list " " . map mkMapper $ ls)
      ++ "\n\n"
    mkMapper l = "(S" ++ show i ++ l : " . " ++ l : ")"

mapDefA :: Int -> String
mapDefA i
  | i == 0 = mapDefA0
  | otherwise = mapDefAN i

mapDefA0 :: String
mapDefA0 = unlines
  [ "mapS0f :: Functor ftr => S0 -> ftr S0"
  , "mapS0f = undefined"
  , ""
  ]

mapDefAN :: Int -> String
mapDefAN i = sig ++ decl
  where
    fName = "mapS" ++ show i ++ "f"
    sig = fName ++ " :: "
      ++ "Functor ftr =>\n"
      ++ "  " ++ list " -> "
      (fns ++ sType i : resType : []) ++ "\n"
    resType = "ftr (" ++ 'S' : show i ++ (' ' : list " " ls')
      ++ ")"
    ls = letters i
    ls' = map (\l -> l : '1' : []) ls
    fns = map mkFn ls
    mkFn l = '(' : l : " -> " ++ "ftr " ++ l : "1)"

    decl = fName ++ " " ++ list " " (map (:[]) ls) ++ " = "
      ++ "caseS" ++ show i ++ " " ++ (list " " . map mkMapper $ ls)
      ++ "\n\n"
    mkMapper l = "(fmap S" ++ show i ++ l : " . " ++ l : ")"

makeModule
  :: String
  -- ^ Module name
  -> Int
  -- ^ Maximum number of type variables
  -> String

makeModule n i = 
  pragmas ++ notice ++ leadHaddocks ++ modName
  ++ imports ++ datas ++ parts
  ++ cases ++ maps ++ mapFs
  where

    pragmas = "{-# LANGUAGE DeriveDataTypeable #-}\n"
      ++ "{-# LANGUAGE EmptyDataDecls #-}\n\n"

    notice = unlines
      [ "-- Text of this module generated by the generate-sums.hs"
      , "-- script, included with the source of the anonymous-sums"
      , "-- package."
      , ""
      ]

    leadHaddocks = unlines
      [ "-- | Anonymous sum types."
      , "--"
      , "-- Provides functionality similar to that of tuples, but"
      , "-- for sum types rather than product types.  Less clumsy"
      , "-- than using nested 'Either'."
      , ""
      ]

    modName = "module " ++ n ++ " where\n\n"

    imports = unlines
      [ "import Data.Typeable"
      , "import Control.Exception"
      , ""
      ]

    datas = "-- * Anonymous sum types\n\n"
      ++ concatMap dataDeclaration vs

    parts = "-- * Partitioning\n\n"
      ++ concatMap partitionDef vs

    cases = "-- * Case analysis\n\n"
      ++ concatMap caseDef vs

    maps = "-- * Mapping\n\n"
      ++ concatMap mapDef vs

    mapFs = "-- * Mapping in a Functor\n\n"
      ++ concatMap mapDefA vs

    vs = [0..i]

usage
  :: String
  -- ^ Program name
  -> String
usage n = unlines
  [ "usage: " ++ n ++ " MODULE_NAME NUM_VARS"
  , "where"
  , "  MODULE_NAME is the name of the module you are"
  , "  creating; e.g. 'Data.Sums'"
  , "  NUM_VARS is the maximum number of type variables"
  , "  you will create."
  , ""
  , "  Output is sent to standard output."
  ]

main :: IO ()
main = do
  as <- getArgs
  pn <- getProgName
  (name, nVars) <- case as of
    x:y:[] -> return (x, read y)
    _ -> IO.hPutStr IO.stderr (usage pn) >> exitFailure
  putStr $ makeModule name nVars