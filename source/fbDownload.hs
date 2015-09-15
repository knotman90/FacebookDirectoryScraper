
import DirectoryUtils
import System.Environment (getArgs)
import Control.Monad
import FacebookScraper

putListLn ::(Show a,Show b, Show c)=> [a] -> [b] -> c -> IO ()
putListLn parameters parameterNames separator= do
		let ll = zip parameterNames  parameters 
		--mapM_  (printPairWithSeparator separator) ll
		mapM_  printPair ll

printPair p = putStrLn $ show p

printPairWithSeparator :: (Show a, Show b, Show c) => c -> (a,b) -> IO () 
printPairWithSeparator sep (f,s) = do
		let ss =  (((show f ) ++ (show sep) ++ (show s)))
		putStrLn ss

parameterNames :: [String]
parameterNames = ["Number of Concurrent scrapers","Scrapers root directory","Threashold for scrapers startup"]
main = do
	[numScrapers,scraperRoot,threasholdScrapLaunch]<-getArgs
	scraperAbsRoot <- absolutize scraperRoot
	let params = [numScrapers,scraperAbsRoot,threasholdScrapLaunch]
	putStrLn "Facebook Directory Downloader"
	putListLn params parameterNames ":"
