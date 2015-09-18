import DirectoryUtils
import System.Environment (getArgs)
import System.Process (proc,createProcess,getProcessExitCode)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Control.Monad as M
import FacebookScraper
import FacebookScraperGlobalDefinitions
import FacebookDirectoryUtils
import Prelude as Pre
import System.Process (ProcessHandle)  
import TorManager(getPid,startTorInstance,terminateTorInstance)
import Data.List as List
import Data.Sequence as Seq
import Data.Traversable  as T  (mapM)
import FacebookScraper (catchAny,downPageCurl)

putListLn ::(Show a,Show b, Show c)=> [a] -> [b] -> c -> IO ()
putListLn parameters parameterNames separator= do
		let ll = Pre.zip parameterNames  parameters 
		--mapM_  (printPairWithSeparator separator) ll
		Pre.mapM_  printPair ll

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
		then undefined --download some links from notOffloadableURIs and recurse
		else do --have enough links to feed processes
				let idxTerminated = Seq.findIndexL (isLeft) processes --check if there is an idle process 
				case idxTerminated of
					--all process busy
					Nothing -> undefined	--wait random seconds and recurse								
					-- process at index idx is ready to do some work
					(Just idx)-> do
						let offUri = head offloadableURIs
						newProcPD<-offloadUri (offUri) idx
						let processedUpdated =update idx (Right (idx,newProcPD)) processes
						masterLoop pd (List.filter (/= offUri) l) numScrapers threasholdScrapLaunch processedUpdated  
					--offload $ head offloadableURIs and recurse


downloadLink :: PROC_DESCRIPTOR -> FBURI -> [FBURI]-> IO ([FBURI],PROC_DESCRIPTOR)
downloadLink procDesc@(scrapID,pd@(ph,pid)) url downloaded = do
	print ("Processing "++url)
	delay <- (randomDelay)
	printf "Waiting %d microseconds\n" ((delay))
	threadDelay (delay)
	html <- catchAny (downPageCurl (scrapID,pd)  url) $ \e -> do
					putStrLn $ "Caught an exception: " ++ show e
					return ""
					
	if (List.null html) 
	then 
		do
			putStrLn "Trying to restart after exception"
			restart
	else
		do		
			links <- extractURIs html
			let pageLinks = List.drop 3 links
			case (List.null pageLinks) of
				True ->restart
				--everything seems ok -> return downloaded link
				False-> return (newLinks links,procDesc)
	
	where 
		newLinks pl 
				|(List.length pl) >=2 = foldr (\v acc ->(fst v):acc) [] pl
				|otherwise		= []
		restart = do
				terminateTorInstance (scrapID,(ph,pid))
				pd'@(ph',pid')<-startTorInstance scrapID
				downloadLink (scrapID,pd') url downloaded	
	
{- 
downItAll (scrapID,pd@(ph,pid)) l@(url:urls) inFileName outFileName = do
		
		M.when (null html) $ do
					putStrLn "Trying to restart after exception"
					restart
		
		links <- extractURIs html
		let pageLinks = drop 3 links
		M.when (null pageLinks) (restart)		
		if isLastLevel ( (snd.last) pageLinks ) 
		then
			do
				appendLinksFile outFileName pageLinks			
				downItAll (scrapID,pd) urls inFileName outFileName
		else
			do
				print ("\t\t cd .RecursiveCall"++url)
				writeFile inFileName ((show l))
				downItAll (scrapID,pd) ((newLinks pageLinks)++urls) inFileName outFileName
	where 
		newLinks pl 
			|(length pl) >=2 = foldr (\v acc ->(fst v):acc) [] pl
			|otherwise		= []
		restart = do
			terminateTorInstance (scrapID,(ph,pid))
			pd'@(ph',pid')<-startTorInstance scrapID
			downItAll (scrapID,pd') l inFileName outFileName	

 -}
 
getUpdateProcessSeq :: Seq (Either ScraperID PROC_DESCRIPTOR) -> IO (Seq (Either ScraperID PROC_DESCRIPTOR))
getUpdateProcessSeq processes =  T.mapM checkProcessState processes 
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
			
																
		
offloadUri :: FBURI -> ScraperID -> IO (ProcessHandle,PID)
offloadUri uri scrapID = do
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
