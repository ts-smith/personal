{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

--MAKE FILE SERVER
--By examining file info record accessors.
--Make File DataType (Model, put in database, do all that is necessary to 
      --make file uploading and downloading api
--Look at yesod.content at mime types, also, sendResponse

import Import
--import Yesod.Form.MassInput
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getHomeR :: Handler RepHtml
getHomeR = do
   defaultLayout $ do
      $(widgetFile "home")

getAssignmentListR :: Handler RepHtml
getAssignmentListR = do
   assignments <- runDB $ selectList [] [Desc AssignmentTitle]
   defaultLayout $ do
      $(widgetFile "assignments")

getPostR :: Handler RepHtml
getPostR = do
   (assignmentsWidget, enctype) <- generateFormPost entryForm
   defaultLayout $ do
      $(widgetFile "postAssignment")
   
postPostR :: Handler RepHtml
postPostR = do
   ((res, assignmentWidget), enctype) <- runFormPost entryForm
   case res of
      FormSuccess validatingAssignment -> do
         let assignment = getAssignment validatingAssignment
         assignmentId <- runDB $ insert assignment
         setMessage $ toHtml $ (assignmentTitle assignment) <> " created"
         redirect $ AssignmentR assignmentId
      _ -> defaultLayout $ do
         setTitle "Please correct your entry form"
         $(widgetFile "assignmentAddError")


getEditR :: AssignmentId -> Handler RepHtml
getEditR theId = do
   assignment <- runDB $ get404 theId
   (editWidget, enctype) <- generateFormPost $ editForm assignment
   defaultLayout $ do
      setTitle $ toHtml $ assignmentTitle assignment
      $(widgetFile "editAssignment")

postEditR :: AssignmentId -> Handler RepHtml
postEditR theId = do
   ((res, assignmentWidget), enctype) <- runFormPost $ entryForm 
   case res of
      FormSuccess validatingAssignment -> do
         let assignment = getAssignment validatingAssignment
         runDB $ replace theId assignment
         setMessage $ toHtml ("*Edited" :: Text)
         redirect $ AssignmentR theId
      _ -> defaultLayout $ do
         setTitle "Please correct your entry form"
         $(widgetFile "assignmentAddError")
   
getAssignmentR :: AssignmentId -> Handler RepHtml
getAssignmentR assignmentId = do
   assignment <- runDB $ get404 assignmentId
   defaultLayout $ do
      setTitle $ toHtml $ assignmentTitle assignment
      $(widgetFile "assignment")

getNewFileR :: Handler RepHtml
getNewFileR = do
   (formWidget, formEnctype) <- generateFormPost fileUploadForm
   defaultLayout $ do
      setTitle "Upload new file."
      [whamlet|
      <form method=post enctype=#{formEnctype}>
         ^{formWidget}
         <input type=submit>|]
 
postNewFileR :: Handler RepHtml
postNewFileR = do
   ((result, formWidget), formEnctype) <- runFormPost fileUploadForm
   case result of
      FormSuccess (fileInfo, _) -> do
         let (fname, ftype, fcontent) = extractFileInfo fileInfo
         bc <- liftIO $ runResourceT $ fcontent $$ (CL.fold (BS.append) BS.empty)
         let newFile = File fname ftype bc
         fid <- runDB $ insert newFile
         defaultLayout $ do [whamlet|file Posted
         <p>#{show bc}|]
      _ -> redirect HomeR

getFileR :: FileId -> Text -> Handler RepHtml
getFileR fid filename = do
   (File n t c) <- runDB $ get404 fid
   let bs = (read $ show t :: BS.ByteString)
   setHeader "Content-Disposition" ("attachment; filename=" `T.append` n)
   sendResponse (bs, toContent c)

   {-defaultLayout $ do [whamlet|
   <p>#{n}
   <p>#{t}
     |]-}
getFilesR :: Handler RepHtml
getFilesR = do
   files <- runDB $ selectList [] [Desc FileFName]
   defaultLayout $ do [whamlet|
   $forall (Entity fid (File n _ _)) <- files
      <p>
         <a href=@{FileR fid n}>#{n}|]


-------- Form -------

data ValidatedAssignment = ValidatedAssignment { getTitle :: Text
                                               , getContent :: Html
                                               , getPassword :: Text
                                               } 

entryForm :: Form ValidatedAssignment
entryForm = renderDivs $ ValidatedAssignment
   <$> areq textField "Title" Nothing
   <*> areq nicHtmlField "Content" Nothing
   <*> areq validPasswordField "Password" Nothing

editForm :: Assignment -> Form ValidatedAssignment
editForm (Assignment title content) = renderDivs $ ValidatedAssignment
   <$> areq textField "Title" (Just title)
   <*> areq nicHtmlField "Content" (Just content)
   <*> areq validPasswordField "Password" Nothing

fileUploadForm :: Form (FileInfo, Text)
fileUploadForm = renderDivs $ (,)
   <$> fileAFormReq "Choose a File"
   <*> areq validPasswordField "Password" Nothing

validPasswordField = checkBool (== "password") ("Bad password" :: Text) passwordField

getAssignment :: ValidatedAssignment -> Assignment
getAssignment (ValidatedAssignment t c _) = Assignment t c

--Util

extractFileInfo :: FileInfo -> (Text, Text, Source (ResourceT IO) BS.ByteString)
extractFileInfo fileInfo = (fileName fileInfo, fileContentType fileInfo, fileSource fileInfo)








--First conduit attempts

--bc <- liftIO $ runResourceT $ fcontent $$ (CB.take 20000)
--bc <- liftIO $ runResourceT $ fcontent $$ CL.consume
--let strictBc = BS.concat bc --strictBc =  BS.concat $ BSL.toChunks bc




{-
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
-}
