import System.Environment
import System.Directory
import Data.List


stripChar ::  Char -> String -> String
stripChar c = filter (/= c)

stripCharFromFile :: Char-> String  -> IO ()
stripCharFromFile char filename = do
	str<-readFile filename
	let newStr= stripChar char str
	writeFile (filename++"stripped") newStr
	putStrLn $ "Processed: "++filename
	
	
getFiles :: String -> String -> IO [String]
getFiles directory prefix = do 
	all <- getDirectoryContents directory
	let filtered = filter (isPrefixOf prefix) all
	return filtered


main = do
	[directory, prefix, char]<-getArgs
	files<-getFiles directory prefix
	let filesN = map (directory ++) files
	putStrLn $show filesN
	let chr = head char
	mapM (stripCharFromFile (chr)) filesN
	putStrLn "END"
