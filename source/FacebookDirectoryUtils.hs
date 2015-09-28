module FacebookDirectoryUtils --(extractURIs,isLastLevel,readURIFromFile,getNumberOfLinkedURI,getWorkerFileName)
 where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List (elemIndex)
import FacebookScraperGlobalDefinitions(FBURI)
import Data.List (isInfixOf)

extractURIs :: String -> IO [(String, String)]
extractURIs html= do
		let doc = readString [withParseHTML yes, withWarnings no] html
		contents <- runX $ doc >>> css "li" >>> css "a" >>> (getAttrValue "href" &&& (deep getText))
		return contents


isLastLevel :: String -> Bool	
isLastLevel name = not $ (elem '-' name) || ("\8211" `isInfixOf` name) ||  ("\12316" `isInfixOf` name)


readURIFromFile :: FilePath -> IO [String]
readURIFromFile fp = do
		queue <- readFile fp
		return (read queue)


--https://www.facebook.com/directory/people/A-96185041-98148000
getNumberOfLinkedURI ::FBURI -> Int
getNumberOfLinkedURI uri =  if not ((null left) || (null right))
							then (read (tail right)) -(read left)
							else (1000000)  --substitute with Int bound
	where 
		(left,right)= getUriInterval uri


createLinkFromFileName :: String -> Char -> FBURI
createLinkFromFileName s letter= base ++ [letter] ++ "-" ++ left ++ (drop 1 right)
	where 
		(left,right) = let (Just idx) = (elemIndex '-' si) in splitAt idx si
		si = drop 6 s
		base= "https://www.facebook.com/directory/people/"
		
createListOfReDownload filename =do
	str <- readFile filename
	let names = lines str
	return $ map createLinkFromFileName names


getWorkerFileName :: FBURI -> String
getWorkerFileName uri = "out_"++(fst int)++"-"++(snd int)
	where int= getUriInterval uri


getUriInterval uri = if not $isLastLevel uri 
					then
						let si = tail $ dropWhile (/= '-') uri -- ([0-9]*)-([0-9]*) 
						in 	
							let (Just idx)=(elemIndex '-' si) --not safe! Nothing patter not covered
							in splitAt  idx si 
					else
						([],[])
