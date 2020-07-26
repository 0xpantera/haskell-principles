module InDepth.DReader where

import Control.Monad.Reader

data Config = Config {
    verbose :: Bool
  }

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True }

type ConfigM = Reader Config

work :: ConfigM ()
work = do
    doSomething

doSomething :: ConfigM ()
doSomething = do
    doSomethingSpecial

beVerbose :: ConfigM ()
beVerbose = pure ()

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
    vrb <- asks verbose
    when vrb beVerbose

silent :: Config -> Config
silent config = config { verbose = False }

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

main :: IO ()
main = do
    config <- getConfiguration
    let result = runReader work config
    print result
