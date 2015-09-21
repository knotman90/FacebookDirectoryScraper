import FacebookScraper 
import System.Environment (getArgs)


main = do
	[urls,inFileName,outFileName,scrapID] <- getArgs
	entryPoint (read scrapID) [urls] inFileName outFileName
	putStrLn ("END")
