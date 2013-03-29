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
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import Database.Persist.GenericSql

import Utils.Utils
import Data.Time.Calendar

--for unsanitaryHtmlField
import Utils.Fields

instance YesodNic App

getHomeR :: Handler RepHtml
getHomeR = do
   authed <- auth
   defaultLayout $ do
      $(widgetFile "home")

getAssignmentListR :: Handler RepHtml
getAssignmentListR = do
   authed <- auth
   assignments <- runDB $ selectList [] [Desc AssignmentPosted]
   defaultLayout $ do
      $(widgetFile "assignments")

getPostR :: Handler RepHtml
getPostR = do
   authed <- auth
   if authed
    then do
      (assignmentWidget, enctype) <- generateFormPost mEntryForm
      defaultLayout $ do
         $(widgetFile "postAssignment")
    else do setMessage donthack
            redirect HomeR
postPostR :: Handler RepHtml
postPostR = do
   authed <- auth
   if authed
    then do 
      ((res, assignmentWidget), enctype) <- runFormPost mEntryForm
      case res of
         FormSuccess validatingAssignment -> do
            day <- liftIO getDay
            let (assignment, files) = getAssignmentWithFiles validatingAssignment day
            assignmentId <- runDB $ insert assignment
            let extract = map extractFileInfo files

            fileContents <- mapM (liftIO . runResourceT . ($$ (CL.fold (BS.append) BS.empty))) $ map (\(_,_,fc)-> fc) extract
         
            let contentModels = map Filecontent fileContents
            contentIds <- mapM (runDB . insert) contentModels
   
            day <- liftIO $ getDay
   
            let fileHeaders = zip extract contentIds
                dbFileHeaders = map (\((fn,ft,_), fid) -> File (Just assignmentId) fn day ft fid) fileHeaders

            mapM_ (runDB . insert) dbFileHeaders
            --fid <- runDB $ insert newFile

            setMessage $ toHtml $ (assignmentTitle assignment) <> " created"
            redirect $ AssignmentR assignmentId
         _ -> do setMessage $ toHtml ("Input Errors" :: Text)
                 defaultLayout $ do
                    setTitle "Please correct your entry form"
                    toWidget [lucius| #message h2 { color: red; }|]
                    $(widgetFile "postAssignment")
    else do
      setMessage donthack
      redirect HomeR


getEditR :: AssignmentId -> Handler RepHtml
getEditR theId = do
   authed <- auth
   if authed
    then do
      assignment <- runDB $ get404 theId
      (editWidget, enctype) <- generateFormPost $ editForm assignment
      defaultLayout $ do
         setTitle $ toHtml $ assignmentTitle assignment
         $(widgetFile "editAssignment")
    else do
      setMessage donthack
      redirect HomeR

postEditR :: AssignmentId -> Handler RepHtml
postEditR theId = do
   authed <- auth
   if authed 
    then do
      ((res, editWidget), enctype) <- runFormPost $ entryForm 
      case res of
         FormSuccess validatingAssignment -> do
            day <- liftIO getDay
            let assignment = getAssignment validatingAssignment day
            runDB $ replace theId assignment
            setMessage $ toHtml ("*Edited" :: Text)
            redirect $ AssignmentR theId
         _ -> do setMessage $ toHtml ("Input Errors" :: Text)
                 defaultLayout $ do
                    setTitle "Please correct your entry form"
                    toWidget [lucius| #message h2 { color: red; }|]
                    $(widgetFile "editAssignment")
    else do
      setMessage donthack
      redirect HomeR
                  

getAssignmentR :: AssignmentId -> Handler RepHtml
getAssignmentR assignmentId = do
   authed <- auth
   --assignment <- runDB $ get404 assignmentId
   let sql = "SELECT ??, ?? FROM assignment LEFT OUTER JOIN file ON (assignment.id = file.assignment_id) WHERE assignment.id = ? LIMIT 3"
       getAssignments :: Handler [(Entity Assignment, Maybe (Entity File))]
       getAssignments = runDB $ rawSql sql [toPersistValue assignmentId]
   queryResult <- getAssignments
   let assignment = entityVal $ fst $  queryResult !! 0
       fileHeaders = map (\ (_, (Just (Entity fid (File _ n _ _ _)))) -> (fid, n) ) $ filter (\ (_, x) -> isJust x) queryResult
       showAttachments = (length fileHeaders) > 0
   defaultLayout $ do
      setTitle $ toHtml $ assignmentTitle assignment
      $(widgetFile "assignment")


getNewFileR :: Handler RepHtml
getNewFileR = do
   redirect HomeR
   {-
   (formWidget, formEnctype) <- generateFormPost fileUploadForm
   defaultLayout $ do
      setTitle "Upload new file."
      [whamlet|
      <form method=post enctype=#{formEnctype}>
         ^{formWidget}
         <input type=submit>|]
         -}
 
postNewFileR :: Handler RepHtml
postNewFileR = do
   authed <- auth
   redirect HomeR
   {-
   ((result, formWidget), formEnctype) <- runFormPost fileUploadForm
   case result of
      FormSuccess (fileInfo, _) -> do
         let (fname, ftype, fcontent) = extractFileInfo fileInfo
         bc <- liftIO $ runResourceT $ fcontent $$ (CL.fold (BS.append) BS.empty)
         contentId <- runDB $ insert $ Filecontent bc
         day <- liftIO $ getDay
         let newFile = File Nothing fname day ftype contentId
         fid <- runDB $ insert newFile
         defaultLayout $ do [whamlet|file Posted
         <p>#{show bc}|]
      _ -> redirect HomeR
   -}


getFileR :: FileId -> Text -> Handler RepPlain
getFileR fid filename = do
   --(File _ n t cid) <- runDB $ get404 fid
   let sql = "SELECT ??, ?? FROM file LEFT OUTER JOIN filecontent ON (file.content_id = filecontent.id) WHERE file.id = ? LIMIT 1"
       getFiles :: Handler [(Entity File, Entity Filecontent)]
       getFiles = runDB $ rawSql sql [toPersistValue fid]
   [((Entity _ (File _ n _ t _)), (Entity _ (Filecontent bc)))] <- getFiles
   let bs = (read $ show t :: BS.ByteString)
   setHeader "Content-Disposition" ("attachment; filename=" `T.append` n)
   sendResponse (bs, toContent bc)

   {-defaultLayout $ do [whamlet|
   <p>#{n}
   <p>#{t}
     |]-}
getFilesR :: Handler RepHtml
getFilesR = do
   authed <- auth
   files <- runDB $ selectList [] [Desc FileFposted]
   defaultLayout $ do [whamlet|
   <ul>
      $forall (Entity fid (File _ n _ _ _)) <- files
         <a class=fileButton href=@{FileR fid n}>
            <li>
               <div>#{n} |]
                      toWidget [lucius| .fileButton div { margin-top: 5px; } |]

getAdminR :: Handler RepHtml
getAdminR = do
   authed <- auth
   if authed
      then do
         defaultLayout $ do [whamlet|
<p>
   <a href=@{PostR}>Post
<p>
   <a href=@{SignoutR}>Signout|]
      else do
         (authWidget, enctype) <- generateFormPost adminForm
         defaultLayout $ do 
            $(widgetFile "signin")

postAdminR :: Handler RepHtml
postAdminR = do 
   ((result, authWidget), enctype) <- runFormPost adminForm
   case result of
      FormSuccess _ -> do
         setSession "authed" "absolutelyAuthed"
         redirect AdminR
      _ -> do
         setMessage $ toHtml ("Wrong!" :: Text)
         defaultLayout $ do
            setTitle "Please correct your entry form"
            toWidget [lucius| #message h2 { color: red; }|]
            $(widgetFile "signin")

getSignoutR :: Handler RepHtml
getSignoutR = do
   setSession "authed" "not"
   redirect HomeR

-------- Form -------

data ValidatedAssignment = ValidatedAssignment { getTitle :: Text
                                               , getContent :: Html
                                               } 
data VassFiles = VassFiles { getMTitle :: Text
                           , getMContent :: Html
                           , getFile1 :: Maybe FileInfo
                           , getFile2 :: Maybe FileInfo
                           , getFile3 :: Maybe FileInfo
                           }

mEntryForm :: Html -> MForm App App (FormResult VassFiles, Widget)
mEntryForm extra = do
   (titleRes, titleView) <- mreq textField "Title" Nothing
   let contentField = unsanitaryNicHtmlField
   (contentRes, contentView) <- mreq contentField "Content" Nothing
   (file1Res, fileAView) <- mopt fileField "Attach a file" Nothing
   (file2Res, fileBView) <- mopt fileField "Attach another file" Nothing
   (file3Res, fileCView) <- mopt fileField "Attach another file" Nothing
   let vassRes = VassFiles <$> titleRes 
                           <*> contentRes
                           <*> file1Res
                           <*> file2Res
                           <*> file3Res
   let widget = do [whamlet|#{extra}
<div id=formContainer>
   <div class=leftField>
      <div class=required>
         <label for=#{fvId titleView}>Title
         ^{fvInput titleView}
      <div class=required>
         <label for=#{fvId contentView}>Content
         ^{fvInput contentView}
   <div class=rightField>
      <div id=form1 class="optional showPost">
         <label for=#{fvId fileAView}>Attach File
         ^{fvInput fileAView}
      <div id=form2 class="optional dontshowPost">
         ^{fvInput fileBView}
      <div id=form3 class="optional dontshowPost">
         ^{fvInput fileCView}
|]
   return (vassRes, widget)

entryForm :: Form ValidatedAssignment
entryForm = renderDivs $ ValidatedAssignment
   <$> areq textField "Title" Nothing
   <*> areq contentField "Content" Nothing
   where contentField = unsanitaryNicHtmlField

editForm :: Assignment -> Form ValidatedAssignment
editForm (Assignment title _ content) = renderDivs $ ValidatedAssignment
   <$> areq textField "Title" (Just title)
   <*> areq contentField "Content" (Just content)
   where contentField = unsanitaryNicHtmlField

fileUploadForm :: Form FileInfo
fileUploadForm = renderDivs $ fileAFormReq "Choose a File"

validPasswordField :: (RenderMessage master FormMessage) => Field sub master Text
validPasswordField = checkBool (== "password") ("Bad password" :: Text) passwordField

adminUserField :: (RenderMessage master FormMessage) => Field sub master Text
adminUserField = checkBool (== "admin") ("Wrong User" :: Text) textField

adminForm :: Form (Text, Text)
adminForm = renderDivs $ (,)
   <$> areq adminUserField "User" Nothing
   <*> areq validPasswordField "Password" Nothing

--Widgets, moved to Foundation.hs

  

--Util

getAssignment :: ValidatedAssignment -> Day -> Assignment
getAssignment (ValidatedAssignment t c) day = Assignment t day c

getAssignmentWithFiles :: VassFiles -> Day -> (Assignment, [FileInfo])
getAssignmentWithFiles (VassFiles t c f1 f2 f3) day = ((Assignment t day c), catMaybes [f1,f2,f3])


extractFileInfo :: FileInfo -> (Text, Text, Source (ResourceT IO) BS.ByteString)
extractFileInfo fileInfo = (fileName fileInfo, fileContentType fileInfo, fileSource fileInfo)

auth :: GHandler s m Bool
auth = do
   authed <- lookupSession "authed"
   if (isJust authed && (fromJust authed) == "absolutelyAuthed")
      then return True
      else return False

donthack :: Html
donthack = toHtml ("don't hack me bro" :: Text)


--Relics
------------------------------------------------------------------------------------------

{-
   something <- runDB $ Esql.select $
                        Esql.from $ \(a `Esql.LeftOuterJoin` mf) -> do
                        Esql.on (Esql.just (a Esql.^. AssignmentId) Esql.==. mf Esql.?. FileAssignmentId)
                        Esql.where_ (a Esql.^. AssignmentId Esql.==. Esql.val assignmentId)
                        return (a,mf)

   joinedDeliverables <- runDB $ runJoin (selectOneMany ((Just FileAssignmentId) <-.) (Just fileAssignmentId))
      { somOrderOne  = [Asc AssignmentTitle]
      , somFilterKeys = (FileAssignmentId <-.)
      , somGetKey = assignmentId
      , somIncludeNoMatch = True
      }
      -}
 

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
