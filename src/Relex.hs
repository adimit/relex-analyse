{-# LANGUAGE Arrows, OverloadedStrings #-}
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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Utf8 as R
import qualified Data.ByteString.Lazy as B
import Text.Blaze.Html5 ((!))
import GHC.Exts (IsString(..))

main :: IO ()
main = do (f:_) <- getArgs
          [pr] <- runX (readDocument [] f >>> process)
          putStrLn $ "Parse has " ++ (show.length $ prResult pr) ++ " sentences."
          putStrLn $ "\tparser used: " ++ prParserName pr
          putStrLn $ "\tparse date : " ++ prParseDate pr
          sentenceHtmls <- mapM makeParseHtml (prResult pr)
          B.writeFile "index.html" (R.renderHtml $ makeHtmlDoc sentenceHtmls)

makeHtmlDoc :: [H.Html] -> H.Html
makeHtmlDoc shs = H.docTypeHtml $ do
    H.head $ H.title "Parse results"
    H.body $ do
        H.p "Parse results"
        foldr (>>) (H.p "") shs

makeParseHtml :: Sentence -> IO H.Html
makeParseHtml (Sentence i s ps) = do
    imgs <- mapM printaparse ps
    return $ do
        H.p.H.string $ "Sentence #"++show i++": "++ s
        foldr (>>) (H.p "") imgs
    where makeName sid pid suffix = "s"++show sid++"p"++show pid++"-"++suffix++".png"
          printaparse (Parse pid ws rels deps) = do
              let (f1,f2) = (makeName i pid "deps",makeName i pid "rels")
              r1 <- runGraphviz (makeAGraph deps ws) Png f1
              r2 <- runGraphviz (makeAGraph rels ws) Png f2
              print (r1,r2)
              return $ H.div $ do
                H.p.H.string $ "Parse #"++show pid
                H.p.H.string $ "Tokens: " ++ unwords (map (show.snd) ws)
                H.div $ do
                    H.p "Dependency graph"
                    H.img ! A.src (fromString f1) ! A.alt (fromString $ "Dependency graph for " ++ s)
                H.div $ do
                    H.p "Relations"
                    H.img ! A.src (fromString f2) ! A.alt (fromString $ "Relation graph for " ++ s)

labeledParameters = nonClusteredParams { fmtNode = \(_,w) -> [Label $ StrLabel (show w)]
                                       , fmtEdge = \(_,_,s) -> [Label $ StrLabel s] }

makeAGraph :: [LEdge String] -> [LNode Word] -> DotGraph Int
makeAGraph es ws = graphToDot labeledParameters (mkGraph (necessaryWords ws) es :: Gr Word String)
    where necessaryWords   = filter (\(i,_) -> S.member i wordsUsedinEdges)
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
    rels    <- readRelns.strip ^<< text <<< tag "relations" -< p
    deps    <- readDepns.strip ^<< text <<< tag "links"     -< p
    returnA -< Parse parseId ws rels deps

readWords :: String -> [LNode Word]
readWords w = (0,Dummy):((lineToWord.split "\t") `map` lines w)
    where lineToWord (i:s:l:p:f:_) = let i' = read i in (i',Word i' s l p f)
          lineToWord (i:s:l:p:_) =   let i' = read i in (i',Word i' s l p "")
          lineToWord x = error $ "Malformed Word!\n" ++ show x

readNumber :: Parser Int
readNumber = liftM read (many1 digit)

readRelns :: String -> [LEdge String]
readRelns s = case parse (relation `sepBy` newline) "relations" s of
                   Left e -> error $ "Failed to parse:\n" ++ show e
                   Right rls -> rls
    where inBrackets p = char '[' *> p <* char ']'
          relation = do name <- many1 (noneOf "(") <* many1 (noneOf "[")
                        w1Id <- inBrackets readNumber
                        w2Id <- many1 (noneOf "[") >> inBrackets readNumber <* many (noneOf "\n")
                        return (w1Id,w2Id,name)

readDepns :: String -> [LEdge String]
readDepns s = case parse (dependency `sepBy` newline) "dependencies" s of
                   Left e -> error $ "Failed to parse:\n" ++ show e
                   Right deps -> deps
    where dependency = do name <- many1 (noneOf "(") <* char '('
                          w1Id <- readNumber
                          w2Id <- string ", " >> readNumber <* many (noneOf "\n")
                          return (w1Id,w2Id,name)

text :: (ArrowXml a) => a XmlTree String
text  = getChildren >>> hasText (not.all isXmlSpaceChar) >>> getText

tag  :: (ArrowXml a) => String -> a XmlTree XmlTree
tag s = getChildren >>> isElem >>> hasName s
