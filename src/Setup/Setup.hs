module Setup.Setup where

import Setup.Programs (programs)
import FileHandling
import System.Directory (createDirectoryIfMissing)

setup :: IO ()
setup = do
    appPath' <- appPath
    createDirectoryIfMissing True (appPath'++"/programs")
    mapM_ (uncurry addProgram) programs
    