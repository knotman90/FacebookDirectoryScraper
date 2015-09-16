
import DirectoryUtils
import System.Environment (getArgs)
import System.Process (proc,createProcess,getProcessExitCode)
import Control.Monad
import FacebookScraper
import FacebookScraperGlobalDefinitions
import FacebookDirectoryUtils
import Prelude as Pre
import System.Process (ProcessHandle)  
import TorManager(getPid)
import Data.List as List
import Data.Sequence as Seq

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

{-
mayLaunchParallelWork :: Int -> Int ->[FBURI] -> Bool
mayLaunchParallelWork numScrapers threasholdScrapLaunch uris
	| Pre.length uris < numScrapers = False
	| otherwise =  all canOffloadFBURI (Pre.take numScrapers uris) -}

canOffloadFBURI :: Int-> FBURI -> Bool
canOffloadFBURI threasholdScrapLaunch e = ((getNumberOfLinkedURI e) < threasholdScrapLaunch) {- (not (isLastLevel e)) && -} 
		
type PROC_DESCRIPTOR = (ScraperID, (ProcessHandle, PID))

masterLoop ::PROC_DESCRIPTOR -> [FBURI] -> Int -> Int -> Seq (Either ScraperID PROC_DESCRIPTOR) -> IO ()
masterLoop _ [] _ _ _= return ()
masterLoop pd l@(url:urls) numScrapers threasholdScrapLaunch processes = do
		newProcesses <- getUpdateProcessSeq processes
		let (offloadableURIs,notOffloadableURIs) = List.partition (canOffloadFBURI threasholdScrapLaunch) l
		if Pre.length offloadableURIs < numScrapers 
		then undefined --download some links from notOffloadableURIs
		else do --have enough links to feed processes
				let idxTerminated = Seq.findIndexL (isLeft) processes
				case idxTerminated of
					--all process busy
					Nothing -> 	undefined								
					-- process at index idx is ready to do some work
					(Just idx) -> undefined --if canOffloadFBURI url


getUpdateProcessSeq :: Seq (Either ScraperID PROC_DESCRIPTOR) -> IO (Seq (Either ScraperID PROC_DESCRIPTOR))
getUpdateProcessSeq processes =  mapM checkProcessState processes 
	where	
		--left me	ans is not running
		--right means is still running
		checkProcessState :: Either ScraperID PROC_DESCRIPTOR -> IO (Either ScraperID PROC_DESCRIPTOR)
		checkProcessState (Right (pd@(scraperID, (ph, _)))) =  do
											exitCode <- getProcessExitCode ph
											case exitCode  of
												Nothing  -> return $ Right pd
												_		 -> return $ Left  scraperID
		checkProcessState left				  = return left										
			
																
		
launchNewProcess :: FBURI -> ScraperID -> IO (ProcessHandle,PID)
launchNewProcess uri scrapID = do
				(_,_,_,ph)<-createProcess (proc "ScraperWorker" [uri,"inputFile",getWorkerFileName uri,show scrapID])
				(_,maybePid) <- getPid ph
				case maybePid of 
					Nothing -> error "error retriving the pid"
					(Just pid) -> return (ph, (read.show) pid) 
				
--masterLoopPause = 2*10^6

isLeft (Left _) = True
isLeft _ 		= False

main = do
	[numScrapers,scraperRoot,threasholdScrapLaunch]<-getArgs
	scraperAbsRoot <- absolutize scraperRoot
	let params = [numScrapers,scraperAbsRoot,threasholdScrapLaunch]
	putStrLn "Facebook Directory Downloader"
	putListLn params parameterNames ":"
