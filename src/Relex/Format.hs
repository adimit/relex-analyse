module Relex.Format where

import Data.Graph.Inductive.Graph

data Word = Word { wIndex    :: Int
                 , wSurface  :: String
                 , wLemma    :: String
                 , wPoS      :: String
                 , wFeatures :: String
                 }
          | Dummy

data Parse = Parse
    { pId      :: Int
    , pWords   :: [LNode Word]
    , pRels    :: [LEdge String]
    , pDeps    :: [LEdge String]
    } deriving Show

data Sentence = Sentence
    { sIndex   :: Int
    , sSurface :: String
    , sParses  :: [Parse]
    } deriving Show

data ParseResult = ParseResult
    { prParserName :: String
    , prParseDate :: String
    , prResult    :: [Sentence]
    } deriving Show

instance Show Word where
    show (Word idx sfc _ _ _) =
        sfc ++ "[" ++ show idx ++ "]"
    show Dummy = "0"
