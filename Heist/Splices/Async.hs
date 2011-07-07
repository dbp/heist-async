{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TemplateHaskell #-}

module Heist.Splices.Async where
  
import            Text.Templating.Heist
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import            Data.Maybe (fromMaybe)

import Heist.Splices.Async.TH (loadJS)

$(loadJS)

heistAsyncSplices = [ ("a-async", aAsync)
                    , ("form-async", formAsync)
                    , ("div-async", divAsync)
                    , ("div-async-append", divAppendAsync)
                    , ("redirect-async", redirectAsync)
                    , ("activate-async", activateAsync)
                    ]

aAsync :: Monad m => Splice m
aAsync = do
  node <- getParamNode
  return [X.setAttribute "rel" "async" $ X.Element "a" (X.elementAttrs node) (X.elementChildren node)]

formAsync :: Monad m => Splice m
formAsync = do
  node <- getParamNode
  return [X.setAttribute "data-async" "1" $ X.Element "form" (X.elementAttrs node) (X.elementChildren node)]


divAsync :: Monad m => Splice m
divAsync = do
  node <- getParamNode
  let name = fromMaybe "undefined" $ X.getAttribute "name" node
  return [X.setAttribute "data-splice-name" name $ X.Element "div" (filter ((/= "name").fst) $ X.elementAttrs node) (X.elementChildren node)]

divAppendAsync :: Monad m => Splice m
divAppendAsync = do
  node <- getParamNode
  let name = fromMaybe "undefined" $ X.getAttribute "name" node
  return [X.setAttribute "data-append-name" name $ X.Element "div" (filter ((/= "name").fst) $ X.elementAttrs node) (X.elementChildren node)]
  
redirectAsync :: Monad m => Splice m
redirectAsync = do
  node <- getParamNode
  case X.getAttribute "url" node of
    Nothing -> return []
    Just url -> return [X.Element "div" [("data-redirect-url", url)] []]


activateAsync :: Monad m => Splice m
activateAsync = do
  -- make sure that only the first call to this does anything.
  modifyTS $ bindSplice "activate-async" (return [])
  return [X.Element "script" [("type","text/javascript")] [X.TextNode js]]
    where js = T.pack fileContents
