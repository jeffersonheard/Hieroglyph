{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  UIBehaviour
-- Copyright   :  2009 Renaissance Computing Institute
-- License     :  BSD3
--
-- Maintainer  :  Jeff Heard <jeff@renci.org>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module UIBehaviour where

import Graphics.Rendering.Hieroglyph.OpenGL
import Graphics.Rendering.Hieroglyph.Primitives
import Graphics.Rendering.Hieroglyph.Visual
import Control.Applicative
import Data.Monoid
import Control.Monad
import App.EventBus
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Graphics.UI.Gtk
import System.Glib.MainLoop
import Data.List (sort)

import WktHieroglyph
import Control.Concurrent
import EventNames
import UIGeometry
import ServerFunctions
import Dialogs
import Debug.Trace
import Try
import System.Process (runCommand)
import Foreign (void)
import Data.IORef

monoidFromMaybe (Just x) = return x
monoidFromMaybe Nothing = mzero

myNothing = Nothing ::(Maybe (Entry, Entry -> IO [EData a]))


parseDialogResponse (dialog : rest) 
    | dialog == "itemSelectionDialogue" = parseItemSelectionResp rest
    | dialog == "singleEntryDialogue" = parseSingleEntryResp rest
    | dialog == "fileSelectionDialogue" = parseFileSelectionResp rest
    | dialog == "contentAdditionDialogue" = parseContentAdditionResp rest
    
parseSingleEntryResp [] = return Nothing
parseSingleEntryResp (lbltxt:x) = produce responseGrp dialogueSrc lbltxt once [EString . unlines $ x] >>= return . Just

parseItemSelectionResp [] = return Nothing
parseItemSelectionResp (lbltxt:x) = produce responseGrp dialogueSrc lbltxt once [EStringL x] >>= return . Just

parseFileSelectionResp [] = return Nothing
parseFileSelectionResp (lbltxt:x) = produce responseGrp dialogueSrc lbltxt once [EString . head $ x] >>= return . Just

parseContentAdditionResp [] = return Nothing
parseContentAdditionResp (lbltxt:_:rest) = produce responseGrp dialogueSrc lbltxt Persistent [EString lbltxt, EString anntxt, EString edata] >>= return . Just
    where (anntxt,(_:rest')) = parseAnnTxt [] rest
          edata = parseEData rest'
          parseAnnTxt a ("</ANNTXT>":x) = a
          parseAnnTxt a (x:xs) = parseAnnTxt (a++('\n':x)) xs
          parseEData a ("</EDATA>":_) = a
          parseEData a (x:xs) = parseEData (a++('\n':x)) xs


runDialog = proc ("." </> "RunDialog.exe")
mapSelectionDialogue maps =  runDialog ("itemSelectionDialogue" : "Select map:" : "False" : maps)
urlDialogue  = runDialog ["singleEntryDialogue", "Connect to map server at url:"]
newConferenceRoomDialogue l = runDialog ["singleEntryDialogue", ",Name your newly created conference room:"] 
enterConferenceRoomDialogue = runDialog ["itemSelectionDialogue" "Select conference room to enter", "False"]++ l
overlayConferenceRoomDialogue l = runDialog ["itemSelectionDialogue", "Select conference room to overlay", "True"]++ l
snapToBookmarkDialogue l= runDialog ["itemSelectionDialogue", "Snap to bookmark", "False"]++ l
addBookmarkDialogue  = runDialog ["singleEntryDialogue", "Name the current viewport bookmark:"] 
addOverlayDialogue  = runDialog ["fileSelectionDialogue", "Select a GeoTIFF", ".tif$"]
addWebServiceDialogue  = runDialog ["singleEntryDialogue", "Type the URL of the webservice to add"] 
addImageDialogue = runDialog ["fileSelectionDialogue", "Select an image icon", "(tif|png|jpg|gif)$"]
addWebLinkDialogue  = runDialog ["contentAdditionDialogue", "Link the current annotation to a URL:"] 
addContentDialogue  = runDialog ["contentAdditionDialogue", "Title of the added content:"] 
addElsewhereDialogue  = runDialog ["contentAdditionDialogue", "Link the current annotation to a bookmark:"]
addElsewhereOtherMapDialogue  = runDialog ["contentAdditionDialogue", "Link the current annotation to a position on another map:"]
selectShapefileDialogue  = runDialog ["itemSelectionDialogue", "Select a data layer" "False"] ++ l

uiResponses b = pollEventGroupWith b selectionGrp $ \event -> do
    let AttributedCoords _ _ namesL = getHieroglyphData . eventdata $ event
        names = Set.fromList namesL
        innames = (flip Set.member) names
        currentURL = head . eventdata <$> defaultURL b
        currentCR = Nothing
        newAnnotations = eventsBySource finishedAnnotationsSrc b /= Set.empty

    shpevent  <- if innames overlayShapefileName then Just <$> (produce requestGrp userDataSrc "raise select shapefile dialogue"  Persistent []) else return Nothing
    mapevent  <- if innames goToMapName then Just <$> (produce requestGrp userDataSrc "raise map selection dialogue" Persistent []) else return Nothing
    urlevent  <- if innames goToUrlName then Just <$> (produce requestGrp userDataSrc "raise url dialogue" Persistent ((monoidFromMaybe currentURL)::[EData HieroglyphGLRuntime]) ) else return Nothing
    ncrevent  <- if innames addConferenceRoomName then Just <$> (produce requestGrp userDataSrc "raise new conference room dialogue" Persistent []) else return Nothing
    ecrevent  <- if innames enterConferenceRoomName  then Just <$> (produce requestGrp userDataSrc "raise enter conference room dialogue" Persistent ((monoidFromMaybe currentCR)::[EData a])) else return Nothing
    ocrevent  <- if innames overlayConferenceRoomName  then Just <$> (produce requestGrp userDataSrc "raise overlay conference room dialogue" Persistent []) else return Nothing
    abevent   <- if innames addBookmarkName  then Just <$> (produce requestGrp userDataSrc "raise add bookmark dialogue" Persistent []) else return Nothing
    daevent   <- if innames deleteAnnotationName  then Just <$> (produce requestGrp userDataSrc "delete next selected annotation" Persistent []) else return Nothing
    haevent   <- if innames hideAnnotationsName  then Just <$> (produce requestGrp userDataSrc "hide annotations" Persistent []) else return Nothing
    nrevent   <- if innames addRectangleName then Just <$> (produce visibleGrp newAnnotationsSrc addRectangleName Persistent [EOther $ Geometry newrect]) else return Nothing
    ncevent   <- if innames addCircleName then Just <$> (produce visibleGrp newAnnotationsSrc addCircleName Persistent [EOther $ Geometry newcircle]) else return Nothing
    npoevent  <- if innames addPolygonName  then Just <$> (produce visibleGrp newAnnotationsSrc addPolygonName Persistent [EOther $ Geometry newpolygon]) else return Nothing
    npaevent  <- if innames addPathName  then Just <$> (produce visibleGrp newAnnotationsSrc addPathName Persistent [EOther $ Geometry newpath]) else return Nothing
    nmevent   <- if innames addMarkerName then Just <$> (produce visibleGrp newAnnotationsSrc addMarkerName Persistent [EOther $ Geometry newmarker]) else return Nothing
    awsevent  <- if innames addWebServiceName  then Just <$> (produce requestGrp userDataSrc "raise add web service dialogue" Persistent []) else return Nothing
    acevent   <- if innames addContentName && newAnnotations  then Just <$> (produce requestGrp userDataSrc addContentDialogNm Persistent []) else return Nothing
    alwevent  <- if innames addWebLinkName && newAnnotations then Just <$> (produce requestGrp userDataSrc addWebLinkDialogNm Persistent []) else return Nothing
    aleevent  <- if innames addElsewhereName && newAnnotations then Just <$> (produce requestGrp userDataSrc addElsewhereDialogNm Persistent []) else return Nothing
    alomevent <- if innames addOtherMapElsewhereName && newAnnotations then Just <$> (produce requestGrp userDataSrc addOtherMapLinkDialogNm Persistent []) else return Nothing
    huievent  <- if innames hideUIName  then Just <$> (produce requestGrp userDataSrc "hide UI" Persistent []) else return Nothing

    let deleteMouseEvent = Deletion . tryJust "ui responses: no mouse event associated" . eventByQName mouseGrp hieroglyphWindowSrc clickNm $ b
        changes = catMaybes [shpevent, mapevent, urlevent
                            , ncrevent, ecrevent, ocrevent
                            , abevent, daevent, haevent
                            , nrevent, ncevent, npoevent
                            , npaevent, nmevent
                            , awsevent, acevent
                            , alwevent, aleevent, alomevent
                            , huievent]
    return $ (Deletion event) : (if length changes == 0 then [] else [deleteMouseEvent]) ++ changes

dialogmap b = Map.fromList
        [("raise map selection dialogue",mapSelectionDialogue maps)
        ,("raise url dialogue",urlDialogue)
        ,("raise new conference room dialogue",newConferenceRoomDialogue)
        ,("raise enter conference room dialogue",enterConferenceRoomDialogue conferencerooms)
        ,("raise overlay conference room dialogue",overlayConferenceRoomDialogue conferencerooms)
        ,("raise select shapefile dialogue",selectShapefileDialogue shapefiles)
        ,("raise snap to bookmark dialogue", snapToBookmarkDialogue bookmarks)
        ,("raise add bookmark dialogue",addBookmarkDialogue)
        ,("raise add web service dialogue",addWebServiceDialogue)
        ,(addWebLinkDialogNm ,addWebLinkDialogue)
        ,(addContentDialogNm ,addContentDialogue)
        ,(addElsewhereDialogNm,addElsewhereDialogue)
        ,(addOtherMapLinkDialogNm,addElsewhereOtherMapDialogue)
        ]

    where shapefiles = fromMaybe [] $ fromEStringL <$> Map.lookup "available shapefiles" viewM
          conferencerooms = fromMaybe [] $ fromEStringL <$> Map.lookup "available conference room names" viewM
          maps = fromMaybe [] $ map fst . fromEAssocL . head . eventdata <$> metadata b
          bookmarks = fromMaybe [] $ map fst . fromEAssocL <$> Map.lookup "available bookmarks" viewM
          viewM = Map.fromList . fromMaybe [] $ viewL
          viewL = fromEAssocL . head . eventdata <$> view b

myFuture :: IO a -> IO (MVar a)
myFuture expression = newEmptyMVar >>= (\mvar -> forkIO (expression >>= putMVar mvar) >> return mvar)
waitOn = takeMVar


dialogueRaisingBehaviour b = pollEventGroupWith b requestGrp $ \event -> do
    if (head . words . ename $ event) == "raise"
     then do
        (_, Just p_stdout, _, ph) <- createProcess $ dialogmap b Map.! (ename event) 
        waitForProcess ph
        resp <- hGetContents p_stdout
        let respEvt = parseDialogResponse . lines $ resp
        widgetDestroy (castToWidget w)
        return $ Deletion event : maybe [] (:[]) respEvt                                
     else
        return []

dialogueResponseBehaviour =
        enterConferenceRoomDialogResponseBehaviour
    |~| shpfileDialogResponseBehaviour
    |~| webSrvcDialogResponseBehaviour
    |~| mapListRequestDialogResponseBehaviour
    |~| mapSelectionDialogResponseBehaviour
    |~| newConferenceRoomDialogResponseBehaviour
    |~| addContentDialogResponseBehaviour
    |~| addWebLinkDialogResponseBehaviour
    |~| addBookmarkDialogResponseBehaviour
    |~| snapToBookmarkDialogResponseBehaviour

webSrvcDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Type the URL of the webservice to add" $ \event -> return $
    let wsuri = fromEString . head . eventdata $ event
        viewL = maybe [] fromEAssocL $ head . eventdata <$> mapView
        viewM = Map.fromList viewL
        viewM' = Map.insert "overlayed webservices" (EStringL $ wsuri : fromEStringL (viewM Map.! "overlayed webservices")) viewM
        viewL' = EAssocL $ Map.toList viewM'
        mapView = view b
    in maybe [] (\mv -> [Insertion mv{ eventdata = [viewL'] }]) mapView

shpfileDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Select a data layer" $ \event -> do
    let wsuri = fromEString . head . eventdata $ event
        viewL = maybe [] fromEAssocL $ head . eventdata <$> mapView
        viewM = Map.fromList viewL
        viewM' = Map.insert "overlayed shapefiles" (EStringL $ wsuri : fromEStringL (viewM Map.! "overlayed shapefiles")) viewM
        viewL' = EAssocL $ Map.toList viewM'
        mapView = view b
    justSelectedE <- produce requestGrp userDataSrc "Just Selected Shapefile" once . eventdata $ event
    return $ maybe [] (\mv -> [Insertion $ mv{ eventdata = [viewL'] }, justSelectedE]) mapView

enterConferenceRoomDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc"Select conference room to enter" $ \event -> return $
    let crname = fromEString . head . eventdata $ event
        viewL = maybe [] fromEAssocL $ head . eventdata <$> mapView
        viewM = Map.fromList viewL
        viewM'= Map.insert "current conference room name" (EString crname) viewM
        viewM'' = Map.insert "current conference room id" (EInt crid) viewM'
        viewL' = EAssocL . Map.toList $ viewM''
        crids = fromEIntL . tryJust "no avilable conference room ids in view" $ Map.lookup "available conference room ids" viewM
        crnames =  fromEStringL . tryJust "no available conference rooms in view" $ Map.lookup "available conference room names" viewM
        cralist = zip crnames crids
        crid = tryJust "conference room id doesn't exist for name" $ lookup crname cralist
        mapView= view b
    in maybe [] (\mv -> [Insertion $ mv{ eventdata = [viewL'] }]) mapView

mapSelectionDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Select map:" $ \event ->
    (let (g,s,n) = selectedMapQName in produce g s n once . eventdata $ event) >>= return . (:[])

mapListRequestDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Connect to map server at url:" $ \event -> do
    mapListRequestE <- let (g,s,n) = requestMapListQName in produce g s n once . eventdata $ event
    serviceURLE <- (let (g,s,n) = serviceURLQName in produce g s n Persistent  .  eventdata $ event)
    return [serviceURLE,mapListRequestE]

newConferenceRoomDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Name your newly created conference room:" $ \event -> do
   let crname = fromEString . head . eventdata $ event
       mapid = fromEInt . tryJust "uiresponses: no map id in view" . lookup "id" . fromEAssocL . head . eventdata . fromJust $ mapView
       mapView = view b
   when (isJust mapView) $ rmtCreateConferenceRoom b mapid crname >> return ()
   return []

addContentDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Title of the added content:" $ \event ->
    let mapView = view b
        viewL = fromEAssocL . head . eventdata . fromJust $ mapView
        maybeCrid = fromEInt <$> lookup "current conference room id" viewL
        newMarkerE = finishedMarker b
        newPathE = finishedPath b
        newCircleE = finishedCircle b
        newRectangleE = finishedRectangle b
        newPolygonE = finishedPolygon b
        newAnnotationE = (listToMaybe . catMaybes $ [newMarkerE, newPathE, newCircleE, newRectangleE, newPolygonE])
     in if (all id [ isJust mapView, isJust maybeCrid, isJust newAnnotationE ])
            then let baseurl = takeWhile (/=':') . fromEString . tryJust "add content dialog resp.: no baseurl in view" . lookup "baseurl" $ viewL
                     crid = tryJust "add content dialog resp.: the impossible happened" maybeCrid
                     Geometry newAnn = fromEOther . head . eventdata . tryJust "add content dialog resp.: the impossible happened" $ newAnnotationE
                     contentpref = fromEString . head . eventdata . tryJust "add content dialog resp.: add content prefix to config file" $ contentPrefix b
                     [_,anntxt,title] = map fromEString . eventdata $ event
                     title' = filter (\alpha -> alpha `Set.member` alphanum)
                     alphanum = Set.fromList (' ':['A'..'z']++['0'..'9'])
                     cleanedUpTitle = contentpref ++ title
                  in do
                    rmtCreateContent b anntxt "content" cleanedUpTitle crid . head . hieroglyph2wkt $ newAnn
                    return [Deletion . fromJust $ newAnnotationE]
            else do
                print (isJust mapView)
                print maybeCrid
                print (isJust newAnnotationE)
                return []

addWebLinkDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Link the current annotation to a URL:" $ \event ->
    let mapView = view b
        viewL = fromEAssocL . head . eventdata . fromJust $ mapView
        maybeCrid = fromEInt <$> lookup "current conference room id" viewL
        newMarkerE = finishedMarker b
        newPathE = finishedPath b
        newCircleE = finishedCircle b
        newRectangleE = finishedRectangle b
        newPolygonE = finishedPolygon b
        newAnnotationE = (listToMaybe . catMaybes $ [newMarkerE, newPathE, newCircleE, newRectangleE, newPolygonE])
     in if (all id [ isJust mapView, isJust maybeCrid, isJust newAnnotationE ])
            then let baseurl = takeWhile (/=':') . fromEString . tryJust "add content dialog resp.: no baseurl in view" . lookup "baseurl" $ viewL
                     crid = tryJust "add content dialog resp.: the impossible happened" maybeCrid
                     Geometry newAnn = fromEOther . head . eventdata . tryJust "add content dialog resp.: the impossible happened" $ newAnnotationE
                     [_,anntxt,title] = map fromEString . eventdata $ event
                  in do
                    rmtCreateContent b anntxt "content" title crid . head . hieroglyph2wkt $ newAnn
                    return [Deletion . fromJust $ newAnnotationE]
            else do
                print (isJust mapView)
                print maybeCrid
                print (isJust newAnnotationE)
                return []


addBookmarkDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Name the current viewport bookmark:" $ \event -> do
   let crname = fromEString . head . eventdata $ event
       mapid = fromEInt . tryJust "uiresponses: no map id in view" . lookup "id" . fromEAssocL . head . eventdata . fromJust $ mapView
       mapView = view b
       maybeCrid = fromEInt <$> lookup "current conference room id" viewL
       viewL = fromEAssocL . head . eventdata . fromJust $ mapView
       [ox,oy] = fromEDoubleL . tryJust "add bookmark response: view doesn't have origin" $ lookup "view origin" viewL
   when (isJust mapView && isJust maybeCrid) $ rmtInsertBookmark b crname (fromJust maybeCrid) ox oy >> return ()
   return []

snapToBookmarkDialogResponseBehaviour b = consumeFullyQualifiedEventWith b responseGrp dialogueSrc "Snap to bookmark" $ \event -> do
    let (w,e,s,n) = tryJust "should be an ortho by this point" . ortho $ hdata
        hdata = getHieroglyphData . eventdata $ renderDataE
        renderDataE = tryJust "snap to bookmark dialog repsonse: no hieroglyph runtime" $ renderData b
        bookmarks = fromEAssocL . tryJust "should be a bookmark list if we got here" . lookup "available bookmarks" . fromEAssocL . head . eventdata . tryJust "no view" . view $ b
        [w',s'] = fromEDoubleL . tryJust "cannot find bookmark in list" . lookup (fromEString . head . eventdata $ event) $ bookmarks
        e' = e-w + w'
        n' = n-s + s'
    return [Insertion renderDataE{ eventdata = setHieroglyphData hdata{ortho = Just (w',e',s',n') } . eventdata $ renderDataE } ]

annotationClickedBehaviour b = pollEventGroupWith b selectionGrp $ \event -> do
    let anns = Set.toList $ eventsBySource annotationsSrc b
        annNames = map ename anns
        annMap = Map.fromList $ zip annNames anns
        AttributedCoords _ _ namesL = getHieroglyphData . eventdata $ event
        selAnns = filter ((flip Map.member) annMap) namesL
        browser = fromEString . head . eventdata . tryJust "Set browser path in config file" $ browserPath b
    forM_ selAnns $ \ann ->
        let annType = fromEString $ annM Map.! "type"
            annUrl = fromEString $ annM Map.! "data"
            annM = Map.fromList . fromEAssocL $ (eventdata $ annMap Map.! ann) !! 1
        in when (annType == "content") . void $ runCommand (browser ++ " " ++ annUrl)
    return []

deleteAnnotationBehaviour b = pollEventGroupWith b selectionGrp $ \event -> do
    let delEvent = eventsByName   "delete next selected annotation" b
        doDelete = delEvent /= Set.empty
        anns = Set.toList $ eventsBySource annotationsSrc b
        annNames = map ename anns
        annMap = Map.fromList $ zip annNames anns
        AttributedCoords _ _ namesL = getHieroglyphData . eventdata $ event
        selAnns = filter ((flip Map.member) annMap) namesL
    when doDelete . forM_ selAnns $ rmtDeleteContent b . fromEInt . fromJust . lookup "id" . fromEAssocL . (!!1) . eventdata . (annMap Map.!)
    if doDelete then return (map Deletion $ event:(Set.toList delEvent) ++ map (annMap Map.!) selAnns) else return []


