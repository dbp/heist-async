{-# LANGUAGE TemplateHaskell #-}
module Heist.Splices.Async.TH 
  (loadJS)
where

import qualified  Data.Text as T
import qualified  Data.Text.IO as TIO
import            Language.Haskell.TH
import            Language.Haskell.TH.Syntax

-- | loadJS: this template haskell function put's the contents of the javascript files into fileContents, so that it can be included with activateAsync
loadJS = do let fname = mkName "fileContents"
            typeSig <- SigD fname `fmap` [t| String |]
            v <- valD (varP fname) (normalB $ loadJSFiles) []
            return [typeSig, v]

loadJSFiles = do fs <- runIO $ mapM readFile ["js/valentine.min.js", "js/reqwest.min.js", "js/qwery.min.js", "js/heist-async.min.js"]
                 lift $ unlines fs
