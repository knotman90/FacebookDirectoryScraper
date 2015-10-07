module TorManager where

import System.IO
import System.Process			(	createProcess,
									waitForProcess,
									terminateProcess,
									proc,
									getProcessExitCode,
									ProcessHandle,
									StdStream( UseHandle ),
									CreateProcess(std_out,std_err)
								)
								
import System.Process.Internals (	withProcessHandle,
									ProcessHandle__(OpenHandle,ClosedHandle),
									PHANDLE
								)
								
								
import Control.Concurrent (threadDelay)

import FacebookScraperGlobalDefinitions

import ScraperConfiguration

{-----------------------------------------------------
Each Scraper work on a differe tor instance in order 
to use a differe tor circuitEach scraper instance 
has its own scraper number. 
The following functions are used to retrive
the per scrap tor ports. -}
getScraperSocksPort:: Int -> Int
getScraperSocksPort scrapID = base_socks_port+scrapID

getScraperControlPort :: Int -> Int
getScraperControlPort scrapID = base_control_port+scrapID



terminateTorInstance :: (ScraperID,(ProcessHandle,PID)) -> IO ()
terminateTorInstance (scrapID,(ph,pid)) = do
	printVerbose ("SCRAP:"++(show scrapID)++" :Terminating tor instance with PID"++(show pid))
	--eraseDirectoryContent ("../data/tor"++(show scrapID))
	terminateProcess ph
	ec<-waitForProcess ph --used to not let OS create Zombie child process thah hang up in the processTable
	printVerbose ("Terminating tor instance with PID"++(show pid)++" with exit code:"++(show ec))
	return ()


createTorProcess :: IsVerbose -> [String]-> IO ProcessHandle
createTorProcess verbose torArgs 
	| verbose = do
		(_,_,_,torph) <-torProc
		return torph
	| otherwise = do --non verbose -> redirecting stderr and out to dev/null
		tmp <- openFile "/dev/null" WriteMode
		(_,_,_,torph) <- createProcess (proc "tor" torArgs){	std_out = UseHandle tmp,
																std_err = UseHandle tmp	}
		return torph
	where
		 torProc= createProcess (proc "tor" torArgs)
	
	


startTorInstance :: ScraperID -> IO (ProcessHandle,PID)
startTorInstance scrapID = do
	printVerbose ("SCRAP:"++(show scrapID)++" :Starting TOR")
	tmp <- openFile "/dev/null" WriteMode
	ph 	<- createTorProcess False torArgs								
	threadDelay (torDelayStartSec)
	exitCode <- getProcessExitCode ph
	case exitCode of
		Nothing -> do
					--highly dangerous
					(_,Just pid) <- getPid ph
					printVerbose ("SCRAP:"++(show scrapID)++" :Tor Instance started succesfully with PID="++(show pid))
					return (ph,read (show pid))
		(Just n) -> do
					printVerbose ("SCRAP:"++(show scrapID)++" :Error Starting tor. Error code:"++(show n)++"\n RESTARTING")
					startTorInstance scrapID	
		-- (Just n) 	-> error ("Error Starting tor. Error code:"++(show n))
	
	where 
		torArgs = ["--RunAsDaemon","0","--CookieAuthentication", "0","--HashedControlPassword","","--ControlPort",show (getScraperControlPort scrapID),"--PidFile",pidFile,"--SocksPort",show (getScraperSocksPort scrapID),"--DataDirectory",dataFolderRoot++(show scrapID)]
		pidFile = let s ="tor"++(show scrapID)	in s++"/"++s++".pid"
	  
getPid ph =  withProcessHandle ph getPID_    

-- | returns Just pid or Nothing if process has already exited
-- for use with withProcessHandle
getPID_ :: ProcessHandle__ -> IO (ProcessHandle__, Maybe PHANDLE)
getPID_ h@(OpenHandle t) = return (h, Just t)
getPID_ h@(ClosedHandle t) = return (h, Nothing)
