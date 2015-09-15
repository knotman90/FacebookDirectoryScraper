module FacebookDirectoryUtils (extractURIs,isLastLevel,readURIFromFile)
 where

import Text.XML.HXT.Core
import Text.HandsomeSoup

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
