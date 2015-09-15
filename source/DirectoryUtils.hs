module DirectoryUtils (absolutize,appendLinksToFile,eraseDirectoryContent) where
--Depends on: MissingH, filepath, directory
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory,getDirectoryContents,removeFile)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Control.Applicative ((<$>))

absolutize :: String -> IO String
absolutize aPath 
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots


eraseDirectoryContent :: FilePath -> IO ()
eraseDirectoryContent dir = do
		--putStrLn ("DELETING ALL FROM :"++(show dir))
		files <-  filter (\f -> not (f `elem` [".",".."])) <$> getDirectoryContents dir
		mapM_ removeFile_ files
	where removeFile_ file = do
						--putStrLn ("Removing:"++(show file))
						removeFile (dir++"/"++file)

appendLinksToFile :: (Show a, Show a1) => FilePath -> [(a1, [a])] -> IO ()
appendLinksToFile filename links = appendFile filename (processLinks links)
	where 
		processLinks ls = concat $ foldr (\v acc -> ("\n"++(show (cleanSnd v))++","++(show (fst v))) : acc) [] ls
		cleanSnd s= tail (snd s)
