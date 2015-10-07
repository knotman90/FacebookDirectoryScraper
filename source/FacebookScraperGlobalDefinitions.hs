module FacebookScraperGlobalDefinitions where

import ScraperConfiguration (verbose)

type UserAgent = String
type ScraperID = Int
type PID = Int
type FBURI = String --facebook URI

type IsVerbose = Bool


printVerbose  s
	| verbose	= print ('\n':s)
	| otherwise = return ()



