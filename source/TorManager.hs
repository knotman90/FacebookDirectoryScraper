module TorManager where

import System.IO
import System.Process	{-		(	createProcess,
									waitForProcess,
									terminateProcess,
									proc,
									getProcessExitCode,
									ProcessHandle,
									StdStream( UseHandle ),
									CreateProcess
								)-}
								
import System.Process.Internals {-(	withProcessHandle,
									ProcessHandle__(OpenHandle,ClosedHandle),
									PHANDLE
								)
								-}
								
import Control.Concurrent (threadDelay)

import FacebookScraperGlobalDefinitions

base_socks_port :: Int
base_socks_port=9050

getScraperSocksPort:: Int -> Int
getScraperSocksPort scrapID = base_socks_port+scrapID

base_control_port :: Int
base_control_port=8118

getScraperControlPort :: Int -> Int
getScraperControlPort scrapID = base_control_port+scrapID


terminateTorInstance :: (ScraperID,(ProcessHandle,PID)) -> IO ()
terminateTorInstance (scrapID,(ph,pid)) = do
	putStrLn ("Terminating tor instance with PID"++(show pid))
	--eraseDirectoryContent ("../data/tor"++(show scrapID))
	terminateProcess ph
	ec<-waitForProcess ph --used to not let OS create Zombie child process thah hang up in the processTable
	putStrLn ("Terminating tor instance with PID"++(show pid)++" with exit code:"++(show ec))
	return ()


dataFolderRoot = "/home/knotman/git/FacebookDirectoryScraper/work/data/tor"

startTorInstance :: ScraperID -> IO (ProcessHandle,PID)
startTorInstance scrapID = do
	putStrLn "Starting TOR"
	tmp <- openFile "/dev/null" WriteMode
	(_,_,_,ph) <-createProcess (proc "tor" torArgs)	
										-- {-	
										{
											std_out = UseHandle tmp,
											std_err = UseHandle tmp
											}
										--	-}
	threadDelay (5*10^6)
	exitCode <- getProcessExitCode ph
	case exitCode of
		Nothing -> do
					--highly dangerous
					(_,Just pid) <- getPid ph
					putStrLn ("Tor Instance "++(show scrapID)++" started succesfully with PID="++(show pid))
					return (ph,read (show pid))
		(Just n) -> do
					putStrLn ("Error Starting tor. Error code:"++(show n)++"\n RESTARTING")
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
