module NetUtils (getPublicIp)
where 

import System.Process (readProcess)

getPublicIp :: IO String
getPublicIp = readProcess "curl" ["-sS","icanhazip.com"] ""
			

