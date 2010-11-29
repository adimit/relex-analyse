{-# LANGUAGE Arrows #-}
module Main where

import Control.Applicative hiding (many, optional)
import Text.XML.HXT.Core
import System.Environment (getArgs)
import Data.Char.Properties.XMLCharProps (isXmlSpaceChar)
import Relex.Format
import Relex.Orphans() -- import orphan instances
import Data.String.Utils (strip)
import Data.List.Utils (split)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Monad (liftM)
import qualified Data.Set as S

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

main :: IO ()
main = do (f:_) <- getArgs
          [pr] <- runX (readDocument [] f >>> process)
          putStrLn $ "Parse has " ++ (show.length $ prResult pr) ++ " sentences."
          putStrLn $ "\tparser used: " ++ prParserName pr
          putStrLn $ "\tparse date : " ++ prParseDate pr
          mapM_ printASentence (prResult pr)

printASentence :: Sentence -> IO ()
printASentence (Sentence i s ps) = do 
    putStrLn $ "Sentence #" ++ show i
    putStrLn $ "\t" ++ show s
    putStrLn $ "\tNumber of parses: " ++ (show.length $ ps)
    mapM_ printaparse ps
    where printaparse p = do let ws = pWords p
                                 rs = pRels p
                                 ds = pDeps p
                                 id = pId p
                             putStrLn $ "\tParse #" ++ show id
                             r1 <- runGraphviz (makeAGraph ds ws) Png (makeName i id "deps")
                             r2 <- runGraphviz (makeAGraph rs ws) Png (makeName i id "rels")
                             print $ (maybeErr r1,maybeErr r2)

makeName :: Int -> Int -> String -> FilePath
makeName sId pId suffix = "s"++show sId++"p"++show pId++"-"++suffix++".png"

makeAGraph :: [LEdge String] -> [LNode Word] -> DotGraph Int
makeAGraph es ws = graphToDot ps (mkGraph ws' es :: Gr Word String)
    where ws' = necessaryWords es ws
          ps = nonClusteredParams { isDirected = True
                                  , fmtNode = \(_,w) -> [Label $ StrLabel (show w)]
                                  , fmtEdge = \(_,_,s) -> [Label $ StrLabel s]
                                  }

necessaryWords :: [LEdge String] -> [LNode Word] -> [LNode Word]
necessaryWords es = filter isNecessary
    where isNecessary (i,_) = S.member i wordsUsedinEdges
          wordsUsedinEdges = foldr (\(i,j,_) s -> S.insert j $ S.insert i s) S.empty es


process :: (ArrowXml a) => a XmlTree ParseResult
process = getChildren >>> proc nlparse -> do
    parser <- text <<< tag "parser" -< nlparse
    date   <- text <<< tag "date"   -< nlparse
    sents  <- listA sentences       -< nlparse
    returnA -< ParseResult parser date sents

sentences :: (ArrowXml a) => a XmlTree Sentence
sentences = tag "sentence" >>> proc sent -> do
    sentId  <- read ^<< getAttrValue "index" -< sent
    surface <- strip ^<< text                -< sent
    parses  <- listA sentenceParse           -< sent
    returnA -< Sentence sentId surface parses

sentenceParse :: (ArrowXml a) => a XmlTree Parse
sentenceParse = tag "parse" >>> proc p -> do
    parseId <- read            ^<< getAttrValue "id"        -< p
    ws      <- readWords.strip ^<< text <<< tag "features"  -< p
    rString <- strip           ^<< text <<< tag "relations" -< p
    dString <- strip           ^<< text <<< tag "links"     -< p
    returnA -< let rels   = readRelns rString
                   deps   = readDepns dString
               in Parse parseId ws rels deps

readWords :: String -> [LNode Word]
readWords w = (0,Dummy):((lineToWord.split "\t") `map` lines w)
    where lineToWord (i:s:l:p:f:_) = let i' = read i in (i',Word i' s l p f)
          lineToWord (i:s:l:p:_) =   let i' = read i in (i',Word i' s l p "")
          lineToWord x = error $ "Malformed Word!\n" ++ show x

readRelns :: String -> [LEdge String]
readRelns s = case parse (relation `sepBy` newline) "relations" s of
                   Left e -> error $ "Failed to parse:\n" ++ show e
                   Right rls -> rls
    where relation = do name <- many1 (noneOf "(") <* many1 (noneOf "[")
                        w1Id <- inBrackets readNumber
                        w2Id <- many1 (noneOf "[") >> inBrackets readNumber <* many (noneOf "\n")
                        return $ (w1Id,w2Id,name)

readDepns :: String -> [LEdge String]
readDepns s = case parse (dependency `sepBy` newline) "dependencies" s of
                   Left e -> error $ "Failed to parse:\n" ++ show e
                   Right deps -> deps
    where dependency = do name <- many1 (noneOf "(") <* char '('
                          w1Id <- readNumber
                          w2Id <- string ", " >> readNumber <* many (noneOf "\n")
                          return $ (w1Id,w2Id,name)

inBrackets :: Parser a -> Parser a
inBrackets p = char '[' *> p <* char ']'

readNumber :: Parser Int
readNumber = liftM read (many1 digit)

-- | Gets non-whitespace text from the node's children.
text :: (ArrowXml a) => a XmlTree String
text  = getChildren >>> hasText (not.all isXmlSpaceChar) >>> getText

-- | Gets the tag with the given name below the node.
tag  :: (ArrowXml a) => String -> a XmlTree XmlTree
tag s = getChildren >>> isElem >>> hasName s
