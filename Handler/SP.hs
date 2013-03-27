{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.SP where

import Import

getStaticOneR :: Handler RepHtml
getStaticOneR = do
   defaultLayout $ do
      $(widgetFile "staticone")
