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
          return ()

printASentence :: Sentence -> IO ()
printASentence (Sentence i s ps) = do 
    putStrLn $ "Sentence #" ++ show i
    putStrLn $ "\t" ++ show s
    putStrLn $ "\tNumber of parses: " ++ (show.length $ ps)
    mapM_ printaparse ps
    where printaparse p = let fname = show i ++ "-" ++ show (pId p) ++ ".png"
                          in do r1 <- runGraphviz (pDepGr p) Png ('d':fname)
                                r2 <- runGraphviz (pRelGr p) Png ('r':fname)
                                print $ (maybeErr r1,maybeErr r2)

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
    returnA -< let ws'    = Dummy:ws
                   rels   = readRelns ws' rString
                   deps   = readDepns ws' dString
                   rGraph = makeGraph ws' (relToNode `map` rels)
                   dGraph = makeGraph ws' (depToNode `map` deps)
               in Parse parseId ws rels deps rGraph dGraph

makeGraph :: [Word] -> [LEdge String] -> DotGraph Node
makeGraph ws es = graphToDot nonClusteredParams (mkGraph (wordToNode `map` ws) es :: Gr String String)

relToNode :: Relation -> LEdge String
relToNode (Relation s (w1,w2)) = (getIndex w1,getIndex w2,s)

depToNode :: Dependency -> LEdge String
depToNode (Link s (w1,w2)) = (getIndex w1,getIndex w2,s)

wordToNode :: Word -> LNode String
wordToNode (Word i s _ _ _) = (i,s)
wordToNode (Dummy         ) = (0,"Root")

readWords :: String -> [Word]
readWords w = (lineToWord.split "\t") `map` lines w
    where lineToWord (i:s:l:p:f:_) = Word (read i) s l p f
          lineToWord (i:s:l:p:_) =   Word (read i) s l p "" -- Call me a lazy ass.
          lineToWord x = error $ "Malformed Word!\n" ++ show x

readRelns :: [Word] -> String -> [Relation]
readRelns ws s = case parse (relation `sepBy` newline) "relations" s of
                         Left e -> error $ "Failed to parse:\n" ++ show e
                         Right rls -> rls
    where relation = do name <- many1 (noneOf "(") <* many1 (noneOf "[")
                        w1Id <- inBrackets readDigit
                        w2Id <- many1 (noneOf "[") >> inBrackets readDigit <* many (noneOf "\n")
                        return $ Relation name (ws!!w1Id,ws!!w2Id)

readDepns :: [Word] -> String -> [Dependency]
readDepns ws s = case parse (dependency `sepBy` newline) "dependencies" s of
                      Left e -> error $ "Failed to parse:\n" ++ show e
                      Right deps -> deps
    where dependency = do name <- many1 (noneOf "(") <* char '('
                          w1Id <- readDigit
                          w2Id <- string ", " >> readDigit <* many (noneOf "\n")
                          return $ Link name (ws!!w1Id,ws!!w2Id)

inBrackets :: Parser a -> Parser a
inBrackets p = char '[' *> p <* char ']'

readDigit :: Parser Int
readDigit = liftM read (many1 digit)

-- | Gets non-whitespace text from the node's children.
text :: (ArrowXml a) => a XmlTree String
text  = getChildren >>> hasText (not.all isXmlSpaceChar) >>> getText

-- | Gets the tag with the given name below the node.
tag  :: (ArrowXml a) => String -> a XmlTree XmlTree
tag s = getChildren >>> isElem >>> hasName s
