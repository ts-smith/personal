{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Random where

import Import
--import Yesod.Form.MassInput
import qualified Data.ByteString as BS
import qualified Data.Text as T

getEvalR :: Handler RepHtml
getEvalR = do
   defaultLayout $ do
      $(widgetFile "eval")
