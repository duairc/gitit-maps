module Network.Gitit.Plugins.Maps (plugin) where

-- base ----------------------------------------------------------------------
import           Control.Monad ((>=>), unless)
import           Data.Char (toLower)
import           Data.List (intercalate, isPrefixOf, unfoldr)
import           Data.Maybe (fromJust)
import           Data.Monoid (mempty)
import           Text.Read (readMaybe)


-- directory -----------------------------------------------------------------
import           System.Directory (createDirectoryIfMissing, doesFileExist)


-- filepath ------------------------------------------------------------------
import           System.FilePath ((</>), (<.>))


-- gd ------------------------------------------------------------------------
import           Graphics.GD.ByteString.Lazy
                     ( copyRegion
                     , copyRegionScaled
                     , imageSize
                     , loadJpegByteString
                     , loadPngByteString
                     , newImage
                     , saveJpegFile
                     , savePngFile
                     )


-- gitit ---------------------------------------------------------------------
import           Network.Gitit
                     ( Plugin (PageTransform)
                     , PluginM
                     , pluginConfig
                     , pluginRequest
                     , staticDir
                     )


-- happstack-server ----------------------------------------------------------
import           Happstack.Server (rqPaths, rqUri)


-- HTTP ----------------------------------------------------------------------
import           Network.HTTP
                     ( Request (Request)
                     , RequestMethod (GET)
                     , rspBody
                     , simpleHTTP
                     , urlEncode
                     )


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Reader (asks)


-- network -------------------------------------------------------------------
import           Network.URI (URI (URI), URIAuth (URIAuth), isUnescapedInURI)


-- pandoc-types --------------------------------------------------------------
import           Text.Pandoc.Definition
                     ( Block (Para, Plain, RawBlock)
                     , Format (Format)
                     , Inline (Image, Link, RawInline, Span)
                     )
import           Text.Pandoc.Walk (walkM)


-- SHA -----------------------------------------------------------------------
import           Data.Digest.Pure.SHA (sha1, showDigest)


-- text ----------------------------------------------------------------------
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)


-- url -----------------------------------------------------------------------
import           Network.URL (decString, encString)


-- xml -----------------------------------------------------------------------
import           Text.XML.Light.Input (parseXMLDoc)
import           Text.XML.Light.Types
                     ( attrKey
                     , attrVal
                     , elAttribs
                     , elName
                     , qName
                     )


------------------------------------------------------------------------------
data MapType = RoadMap | Satellite | Hybrid
  deriving (Show)


------------------------------------------------------------------------------
data Map
    = Map MapType (Double, Double) Double Bool
    | StreetView (Double, Double) Double Double Double Bool
  deriving (Show)


------------------------------------------------------------------------------
isHalfSize :: Map -> Bool
isHalfSize (Map _ _ _ s) = s
isHalfSize (StreetView _ _ _ _ s) = s


------------------------------------------------------------------------------
parseMap :: String -> Maybe Map
parseMap string = do
    element <- parseXMLDoc string
    let name = qName (elName element)
    let attrs = map (\a -> (qName (attrKey a), attrVal a)) (elAttribs element)
    case name of
         "map" -> parseMap RoadMap attrs
         "hybrid" -> parseMap Hybrid attrs
         "satellite" -> parseMap Satellite attrs
         "streetview" -> parseStreetView attrs
         _ -> Nothing
  where
    parseMap maptype attrs = do
        center <- lookup "location" attrs >>= readMaybe . (\a -> '(':a ++ ")")
        zoom <- lookup "zoom" attrs >>= readMaybe
        let size = maybe False (== "half") (lookup "size" attrs)
        return $ Map maptype center zoom size
    parseStreetView attrs = do
        center <- lookup "location" attrs >>= readMaybe . (\a -> '(':a ++ ")")
        heading <- lookup "heading" attrs >>= readMaybe
        pitch <- lookup "pitch" attrs >>= readMaybe
        zoom <- lookup "zoom" attrs >>= readMaybe
        let size = maybe False (== "half") (lookup "size" attrs)
        return $ StreetView center zoom heading pitch size


------------------------------------------------------------------------------
getImage :: Map -> PluginM FilePath
getImage map_ = do
    static <- asks (staticDir . pluginConfig)
    liftIO $ createDirectoryIfMissing True (static </> "maps")
    let (url, file, transform) = case map_ of
         Map maptype location zoom half -> mapUrl maptype location zoom half
         StreetView location zoom heading pitch half ->
            streetViewUrl location zoom heading pitch half
    let file' = static </> file
    exists <- liftIO $ doesFileExist file'
    liftIO $ unless exists $ do
        let rq = Request url GET [] mempty
        body <- simpleHTTP rq >>= either (fail . show) (return . rspBody)
        transform file' body
    return $ '/':file
  where
    google p q = URI "http:" (Just (URIAuth "" "maps.googleapis.com" "")) p q ""
    mapUrl maptype (lat, long) zoom half = do
        let center = urlEncode $ show lat ++ "," ++ show long
        let zoom' = urlEncode $ show $ round $ 3 + (1.8 * zoom)
        let maptype' = urlEncode $ map toLower $ show $ maptype
        let scale = if half then "1" else "2"
        let size = if half then "335x429" else "340x320"
        let format = case maptype of {RoadMap -> "png"; _ -> "jpg"}
        let query = concat
             [ "?center="
             , center
             , "&zoom="
             , zoom'
             , "&maptype="
             , maptype'
             , "&scale="
             , scale
             , "&size="
             , size
             , "&format="
             , format
             , "&sensor=false"
             ]
        let path = "/maps/api/staticmap"
        let url = google path query
        let file = "maps" </> sha1sum (path ++ query) <.> format
        let transform file' body = do
             let size' = if half then (335, 207) else (680, 414)
             image <- case maptype of
                RoadMap -> loadPngByteString body
                _ -> loadJpegByteString body
             image' <- newImage size'
             copyRegion (0, 113) size' image (0, 0) image'
             case maptype of
                RoadMap -> savePngFile file' image'
                _ -> saveJpegFile 95 file' image'
        (url, file, transform)
    streetViewUrl (lat, long) zoom heading pitch half = do
        let location = urlEncode $ show lat ++ "," ++ show long
        let heading' = urlEncode $ show $ heading
        let pitch' = urlEncode $ show $ pitch
        let fov = urlEncode $ show $ 120 - 110 * zoom / 10
        let size = if half then "335x457" else "640x640"
        let query = concat
             [ "?location="
             , location
             , "&heading="
             , heading'
             , "&pitch="
             , pitch'
             , "&fov="
             , fov
             , "&size="
             , size
             , "&sensor=false"
             ]
        let path = "/maps/api/streetview"
        let url = google path query
        let file = "maps" </> sha1sum (path ++ query) <.> "jpg"
        let transform file' body = do
             let size' = if half then (335, 207) else (680, 414)
             image <- loadJpegByteString body
             image' <- newImage size'
             if half then do
                 copyRegion (0, 125) size' image (0, 0) image'
             else do
                 copyRegionScaled (0, 125) (640, 390) image (0, 0) size' image'
             saveJpegFile 95 file' image'
        (url, file, transform)
    sha1sum = showDigest . sha1 . encodeUtf8 . pack


------------------------------------------------------------------------------
getLink :: Map -> String
getLink (Map maptype (lat, long) zoom _) = do
    let location = urlEncode $ show lat ++ "," ++ show long
    let zoom' = urlEncode $ show $ round (3 + (1.8 * zoom))
    let type_ = case maptype of
         RoadMap -> "m"
         Satellite -> "k"
         Hybrid -> "h"
    concat
        [ "https://maps.google.ie/maps?ll="
        , location
        , "&z="
        , zoom'
        , "&t="
        , type_
        ]
getLink (StreetView (lat, long) heading pitch zoom _) = do
    let location = urlEncode $ show lat ++ "," ++ show long
    let heading' = urlEncode $ show $ heading
    let pitch' = urlEncode $ show $ negate pitch
    let zoom' = show $ if zoom <= 4 then 0 else if zoom > 7 then 2 else 1
    concat
        [ "https://maps.google.ie/maps?ll="
        , location
        , "&layer=c&z=15&cbll="
        , location
        , "&cbp=12,"
        , heading'
        , ",,"
        , zoom'
        , ","
        , pitch'
        ]


------------------------------------------------------------------------------
getInline :: Map -> PluginM Inline
getInline map_ = do
    file <- getImage map_
    base <- getWikiBase
    let imageUrl = base ++ encString False isUnescapedInURI file
    let alt = case map_ of
         StreetView _ _ _ _ _ -> "Street view imagery"
         Map RoadMap _ _ _ -> "Map"
         Map Satellite _ _ _ -> "Satellite view"
         Map Hybrid _ _ _ -> "Combined map/satellite view"
    let image = Image [] (imageUrl, alt)
    let class_ = if isHalfSize map_ then "halfsize" else "fullsize"
    let span_ = Span ([], [class_], []) [image]
    let link = getLink map_
    return $ Link [span_] (link, "")


------------------------------------------------------------------------------
inlines :: Inline -> PluginM Inline
inlines i@(RawInline (Format "html") x) =
    maybe (return i) getInline (parseMap x)
inlines i = return i


------------------------------------------------------------------------------
blocks :: Block -> PluginM Block
blocks b@(RawBlock (Format "html") x) = case parseMap x of
    Nothing -> return b
    Just map_ -> fmap (\i -> Plain [i]) (getInline map_)
blocks b@(Para [i@(RawInline (Format "html") _)]) =
    fmap (\i' -> Plain [i']) (inlines i)
blocks b = return b


normalizeFigure :: Block -> PluginM Block
normalizeFigure (RawBlock h@(Format "html") ('<':'f':'i':'g':'u':'r':'e':'>':rest)) = do
    return $ Plain [RawInline h "<figure>", RawInline h rest]
normalizeFigure b = return b


------------------------------------------------------------------------------
getWikiBase :: PluginM String
getWikiBase = do
    rq <- asks pluginRequest
    let path' = fromJust . decString False . intercalate "/" $ rqPaths rq
    let uri' = fromJust . decString False $ rqUri rq
    case calculateWikiBase path' uri' of
        Just b -> return b
        Nothing -> error $ "Could not getWikiBase: (path, uri) = " ++
            show (path', uri')
  where
    calculateWikiBase path' uri' = do
        let revpaths = reverse . filter (not . null) $ splitOn '/' path'
        let revuris  = reverse . filter (not . null) $ splitOn '/' uri'
        if revpaths `isPrefixOf` revuris
            then do
                let revbase = drop (length revpaths) revuris
                let revbase' = case revbase of
                     (x:xs) | startsWithUnderscore x -> xs
                     xs -> xs
                let base' = intercalate "/" $ reverse revbase'
                return $ if null base' then "" else '/' : base'
            else Nothing
    startsWithUnderscore ('_':_) = True
    startsWithUnderscore _ = False
    splitOn = unfoldr . go
      where
        go _ [] = Nothing
        go x xs = let (h, t) = span (/= x) xs in Just (h, drop 1 t)


------------------------------------------------------------------------------
plugin :: Plugin
plugin = PageTransform $ walkM normalizeFigure >=> walkM blocks >=> walkM inlines
