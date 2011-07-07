{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TemplateHaskell #-}

module Heist.Splices.Async where
  
import            Text.Templating.Heist
import qualified  Data.Text as T
import qualified  Text.XmlHtml as X
import            Data.Maybe (fromMaybe)

import Heist.Splices.Async.TH (loadJS)

$(loadJS)

-- | heistAsyncSplices provides the six basic splices with sensible defaults
-- These are:
-- a-async: a link that loads it's results asynchronously and replaces parts of the page based on the contents. A normal anchor tag in all ways.
-- form-async: a form that submits asynchronously and replaces parts of the page with the results. A normal form tag otherwise.
-- div-async: a div that can be replaced or replace content on the page. It takes a "name" attribute that is it's unique identifier. When sending back content to replace, any div-asyncs present will replace existing div-asyncs on the page (identified by the name attribute)
-- div-async-append: a special div-async that instead of replacing the corresponding one on the page, it appends it's contents inside the existing div-async-append. Note: div-async's and div-async-appends are not interchangeable. This is so that it is easy to see what is going to happen from looking at the templates. If you need this jund of behavior, wrap you div-async-append inside a div-async.
-- redirect-async: this tag allows you to cause a client-side redirect. This is necessary because if you do a regular redirect, it will be followed by the browser and the result (the new page) will be handed back as if it were the page fragment response. It takes a "url" attribute that specifies where to redirect to.
-- activate-async: this is a convenience tag that will include all the necessary javascript. Feel free to copy the files yourself from tho js directory - by having separate files, they can be cached, which will mean less network transfer. Of course, the intention with this tag is you can get this running as quickly as possible. It can occur any number of times on the page, but will only actually include the javascript the first time.
heistAsyncSplices = [ ("a-async", aAsync)
                    , ("form-async", formAsync)
                    , ("div-async", divAsync)
                    , ("div-async-append", divAppendAsync)
                    , ("redirect-async", redirectAsync)
                    , ("activate-async", activateAsync)
                    ]
-- | aAsync: the actual splice used for "a-async" in heistAsyncSplices, in case you want to bind it to another tag.
aAsync :: Monad m => Splice m
aAsync = do
  node <- getParamNode
  return [X.setAttribute "rel" "async" $ X.Element "a" (X.elementAttrs node) (X.elementChildren node)]

-- | formAsync: the actual splice used for "form-async" in heistAsyncSplices, in case you want to bind it to another tag.
formAsync :: Monad m => Splice m
formAsync = do
  node <- getParamNode
  return [X.setAttribute "data-async" "1" $ X.Element "form" (X.elementAttrs node) (X.elementChildren node)]


-- | divAsync: the actual splice used for "div-async" in heistAsyncSplices, in case you want to bind it to another tag.
divAsync :: Monad m => Splice m
divAsync = do
  node <- getParamNode
  let name = fromMaybe "undefined" $ X.getAttribute "name" node
  return [X.setAttribute "data-splice-name" name $ X.Element "div" (filter ((/= "name").fst) $ X.elementAttrs node) (X.elementChildren node)]

-- | divAsyncAppend: the actual splice used for "div-async-append" in heistAsyncSplices, in case you want to bind it to another tag.
divAppendAsync :: Monad m => Splice m
divAppendAsync = do
  node <- getParamNode
  let name = fromMaybe "undefined" $ X.getAttribute "name" node
  return [X.setAttribute "data-append-name" name $ X.Element "div" (filter ((/= "name").fst) $ X.elementAttrs node) (X.elementChildren node)]
  
-- | redirectAsync: the actual splice used for "redirect-async" in heistAsyncSplices, in case you want to bind it to another tag.
redirectAsync :: Monad m => Splice m
redirectAsync = do
  node <- getParamNode
  case X.getAttribute "url" node of
    Nothing -> return []
    Just url -> return [X.Element "div" [("data-redirect-url", url)] []]


-- | activateAsync: the actual splice used for "activate-async" in heistAsyncSplices, in case you want to bind it to another tag.
activateAsync :: Monad m => Splice m
activateAsync = do
  -- make sure that only the first call to this does anything.
  modifyTS $ bindSplice "activate-async" (return [])
  return [X.Element "script" [("type","text/javascript")] [X.TextNode js]]
    where js = T.pack fileContents
