module ScraperConfiguration where

base_control_port :: Int
base_control_port=8118

base_socks_port :: Int
base_socks_port=9050


pathScraperWorkerExe = "/home/knotman/git/FacebookDirectoryScraper/dist/build/ScraperWorker/ScraperWorker"
dataFolderRoot = "/home/knotman/git/FacebookDirectoryScraper/work/data/tor"

verbose = True


--controls the waiting time before to check wether tor has started correcly
torDelayStartSec :: Int
torDelayStartSec =      let wait=5 
						in
						wait * 10^6

--modify wait to change random wait-> in seconds
scrapUppBoundWaitSec :: Int
scrapUppBoundWaitSec = let wait=5 
						in
						wait * 10^6
intUpperBound :: Int
intUpperBound = 10^6
