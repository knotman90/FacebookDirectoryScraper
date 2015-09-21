module FacebookDirectoryUtils --(extractURIs,isLastLevel,readURIFromFile,getNumberOfLinkedURI,getWorkerFileName)
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
getNumberOfLinkedURI ::FBURI -> Int
getNumberOfLinkedURI uri =  (read (tail right)) -(read left)
	where 
		(left,right)= getUriInterval uri


getWorkerFileName :: FBURI -> String
getWorkerFileName uri = "out_"++(fst int)++"-"++(snd int)
	where int= getUriInterval uri


getUriInterval uri = 	let si = tail $ dropWhile (/= '-') uri -- ([0-9]*)-([0-9]*) 
						in 	
							let (Just idx)=(elemIndex '-' si) --not safe! Nothing patter not covered
							in splitAt  idx si 
