module FacebookDirectoryUtils (extractURIs,isLastLevel,readURIFromFile,getNumberOfLinkedURI)
 where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List (elemIndex)
import FacebookScraperGlobalDefinitions(FBURI)

extractURIs :: String -> IO [(String, String)]
extractURIs html= do
		let doc = readString [withParseHTML yes, withWarnings no] html
		contents <- runX $ doc >>> css "li" >>> css "a" >>> (getAttrValue "href" &&& (deep getText))
		return contents


isLastLevel :: String -> Bool	
isLastLevel name = not $ elem '-' name


readURIFromFile :: FilePath -> IO [String]
readURIFromFile fp = do
		queue <- readFile fp
		return (read queue)


--https://www.facebook.com/directory/people/A-96185041-98148000
getNumberOfLinkedURI ::(Integral a,Read a) => FBURI -> a
getNumberOfLinkedURI uri =  (read (tail right)) -(read left)
	where 
		si = tail $ dropWhile (/= '-') uri -- ([0-9]*)-([0-9]*)
		(right,left)= let (Just idx)=(elemIndex '-' si) in splitAt  idx si --not safe! Nothing patter not covered
