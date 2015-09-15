
import DirectoryUtils
import System.Environment (getArgs)
import Control.Monad
import FacebookScraper
import FacebookScraperGlobalDefinitions
import FacebookDirectoryUtils
import Data.Sequence as Seq
import Prelude as Pre
import System.Process (ProcessHandle)  

putListLn ::(Show a,Show b, Show c)=> [a] -> [b] -> c -> IO ()
putListLn parameters parameterNames separator= do
		let ll = Pre.zip parameterNames  parameters 
		--mapM_  (printPairWithSeparator separator) ll
		mapM_  printPair ll

printPair p = putStrLn $ show p

printPairWithSeparator :: (Show a, Show b, Show c) => c -> (a,b) -> IO () 
printPairWithSeparator sep (f,s) = do
		let ss =  (((show f ) ++ (show sep) ++ (show s)))
		putStrLn ss

parameterNames :: [String]
parameterNames = ["Number of Concurrent scrapers","Scrapers root directory","Threashold for scrapers startup"]


mayLaunchParallelWork :: Int -> Int ->[FBURI] -> Bool
mayLaunchParallelWork numScrapers threasholdScrapLaunch uris
	| Pre.length uris < numScrapers = False
	| otherwise =  all canOffloadFBURI (Pre.take numScrapers uris) 
	where 
		canOffloadFBURI e= (not (isLastLevel e)) && ((getNumberOfLinkedURI e) < threasholdScrapLaunch)
		
type PROC_DESCRIPTOR = (PID, (ProcessHandle, ScraperID))
masterLoop ::PROC_DESCRIPTOR -> [FBURI] -> Int -> Int -> Seq (Either ScraperID PROC_DESCRIPTOR) -> IO ()
masterLoop _ [] _ _ = undefined
masterLoop pd l@(url:urls) numScrapers threasholdScrapLaunch 
		| mayLaunchParallelWork numScrapers  threasholdScrapLaunch l = undefined
		| otherwise = undefined
			

main = do
	[numScrapers,scraperRoot,threasholdScrapLaunch]<-getArgs
	scraperAbsRoot <- absolutize scraperRoot
	let params = [numScrapers,scraperAbsRoot,threasholdScrapLaunch]
	putStrLn "Facebook Directory Downloader"
	putListLn params parameterNames ":"
