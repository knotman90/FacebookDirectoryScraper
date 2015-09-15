import FacebookScraper 
import System.Environment (getArgs)


main = do
	[urls,inFileName,outFileName,scrapID] <- getArgs
	entryPoint (read scrapID) (read urls) inFileName outFileName
	putStrLn ("END")
