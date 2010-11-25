module Relex.Format where

import Data.GraphViz
import Data.Graph.Inductive.Graph

data Word = Word { wIndex    :: Int
                 , wSurface  :: String
                 , wLemma    :: String
                 , wPoS      :: String
                 , wFeatures :: String
                 }
          | Dummy

getIndex (Word i _ _ _ _) = i
getIndex (Dummy)          = 0

data Parse = Parse
    { pId      :: Int
    , pWords   :: [Word]
    , pRelns   :: [Relation]
    , pDepcs   :: [Dependency]
    , pDepGr   :: DotGraph Node
    , pRelGr   :: DotGraph Node
    } deriving Show

data Sentence = Sentence
    { sIndex   :: Int
    , sSurface :: String
    , sParses  :: [Parse]
    } deriving Show

data Dependency = Link     String (Word,Word) deriving Show
data Relation   = Relation String (Word,Word) deriving Show

data ParseResult = ParseResult
    { prParserName :: String
    , prParseDate :: String
    , prResult    :: [Sentence]
    } deriving Show

instance Show Word where
    show (Word idx sfc lmm pos ftr) =
        sfc ++ "[" ++ show idx ++ "]"
    show Dummy = "0"