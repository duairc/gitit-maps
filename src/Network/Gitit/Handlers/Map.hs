{-# LANGUAGE DeriveDataTypeable #-}

module Network.Gitit.Handlers.Map
    ( getMapConfig
    , mapPage
    )
where

-- base ----------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Exception (handle, throwIO)
import           Control.Monad (guard, mplus, when)
import           Data.Function (on)
import           Data.List (sort, sortBy)
import           Data.Char (toUpper)
import           Data.Maybe (isJust, isNothing, mapMaybe)
import           Data.Traversable (for, sequence)
import           Prelude hiding (sequence)
import           System.IO (IOMode (ReadMode), hGetLine, withFile)
import           System.IO.Error (isDoesNotExistError, isEOFError)
import           Text.Read (readMaybe)


-- ConfigFile ----------------------------------------------------------------
import           Data.ConfigFile (content, emptyCP, get, readfile)


-- containers ----------------------------------------------------------------
import           Data.Map
                     ( Map
                     , delete
                     , elems
                     , fromList
                     , insert
                     , mapAccum
                     , mapWithKey
                     , toList
                     , union
                     )
import qualified Data.Map as M (null, lookup)


-- directory -----------------------------------------------------------------
import           System.Directory (doesFileExist)


-- filepath ------------------------------------------------------------------
import           System.FilePath
                     ( (</>)
                     , (<.>)
                     , dropExtension
                     , makeRelative
                     , takeDirectory
                     )


-- filestore -----------------------------------------------------------------
import           Data.FileStore (index)


-- gitit ---------------------------------------------------------------------
import           Network.Gitit


-- json ----------------------------------------------------------------------
import           Text.JSON
                     ( JSON (readJSON, showJSON)
                     , JSValue
                         ( JSArray
                         , JSBool
                         , JSRational
                         , JSObject
                         , JSString
                         )
                     , Result (Error, Ok)
                     , fromJSObject
                     , fromJSString
                     , showJSValue
                     , toJSObject
                     , toJSString
                     )


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)


-- network -------------------------------------------------------------------
import           Network.URI (isUnescapedInURI)


-- parsec --------------------------------------------------------------------
import           Text.ParserCombinators.Parsec
                     ( char
                     , digit
                     , letter
                     , many
                     , newline
                     , noneOf
                     , notFollowedBy
                     , oneOf
                     , parse
                     , skipMany
                     , skipMany1
                     , try
                     )


-- url -----------------------------------------------------------------------
import           Network.URL (encString)


-- xhtml ---------------------------------------------------------------------
import           Text.XHtml
                     ( (!)
                     , (<<)
                     , (+++)
                     , alt
                     , checkbox
                     , checked
                     , fieldset
                     , form
                     , identifier
                     , image
                     , label
                     , legend
                     , li
                     , noHtml
                     , src
                     , strAttr
                     , thediv
                     , thefor
                     , thespan
                     , title
                     , ulist
                     )


------------------------------------------------------------------------------
data MapConfig = MapConfig
    { _showOnlyConfiguredCategories :: Bool
    , _hidePlacelessCategories :: Bool
    , _mapCategories :: Maybe Categories
    }
  deriving (Show)


------------------------------------------------------------------------------
getMapConfig :: Config -> IO MapConfig
getMapConfig cfg = handle handler $ do
    let path = takeDirectory (userFile cfg) </> "gitit-maps-config"
    cp <- readfile emptyCP path >>= either (fail . show) return
    let hidePlaceless = maybe' True id $
         get cp "DEFAULT" "hide-placeless-categories"
    let onlyConfigured = maybe' True id $
         get cp "DEFAULT" "show-only-configured-categories"
    categories <- do
        let toCategory k _ = do
             let name = maybe' (capitalize k) id $ get cp k "name"
             let icon = maybe' Nothing Just $ get cp k "icon"
             let checked_ = maybe' True id $ get cp k "show-by-default"
             (name, icon, checked_)
        let assoc = delete "DEFAULT" (content cp)
        if M.null assoc then return Nothing else fmap Just $ do
            assignDefaultIcons cfg (mapWithKey toCategory assoc)
    return $ MapConfig onlyConfigured hidePlaceless categories
  where
    maybe' k f = either (const k) f
    handler e
        | isDoesNotExistError e = return $ MapConfig True True Nothing
        | otherwise = throwIO e


------------------------------------------------------------------------------
assignDefaultIcons
    :: Config
    -> Map String (String, Maybe FilePath, Bool)
    -> IO Categories
assignDefaultIcons cfg tuples = do
    let static = staticDir cfg
    tuples' <- sequence . flip mapWithKey tuples $ \k (n, i, c) -> do
        exists <- doesFileExist $ static </> "img" </> "icons" </> k <.> "png"
        let i' = guard exists >> return ("/img/icons/" ++ k ++ ".png")
        return (n, mplus i i', c)
    let noIcons = length $ filter (\(_, i, _) -> isNothing i) (elems tuples')
    let j = if noIcons > 12 then 1 else 12 / fromIntegral noIcons
    let go x (n, Nothing, c) = (x + j, Category n (generic x) c)
        go x (n, Just i, c) = (x, Category n i c)
    return $ snd $ mapAccum go (0 :: Double) tuples'
  where
    generic i = concat
        [ "/img/icons/generic-"
        , show ((round i `mod` 12 :: Int) + 1)
        , ".png"
        ]


------------------------------------------------------------------------------
capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize x = x


------------------------------------------------------------------------------
getCategories :: MapConfig -> GititServerPart Categories
getCategories mcfg@(MapConfig onlyConfigured _ categories) = do
    almostAll <- case categories of
        Nothing -> getAllCategories mcfg
        Just selectedCategories -> if onlyConfigured
            then return selectedCategories
            else do
                allCategories <- getAllCategories mcfg
                return $ union selectedCategories allCategories
    --let other = Category "(other)" "/img/icons/generic-0.png" False
    return $ insert "(other)" other almostAll


------------------------------------------------------------------------------
getAllCategories :: MapConfig -> GititServerPart Categories
getAllCategories mcfg = do
    cfg <- getConfig
    files <- getFileList
    categories <- liftIO $ fmap concat $ for files $ \file -> do
        (categories, location) <- readCategoriesAndLocation file
        return $ do
            when (_hidePlacelessCategories mcfg) $ guard $ isJust location
            categories
    liftIO $ assignDefaultIcons cfg $ fromList $
        map (\k -> (k, (capitalize k, Nothing, True))) categories


------------------------------------------------------------------------------
type Categories = Map String Category


------------------------------------------------------------------------------
data Category = Category
    { _name :: String
    , _icon :: FilePath
    , _checked :: Bool
    }
  deriving (Show)


------------------------------------------------------------------------------
instance JSON Category where
    showJSON (Category name icon showByDefault) = JSObject $ toJSObject $
        [ ("name", JSString $ toJSString name)
        , ("icon", JSString $ toJSString icon)
        , ("showByDefault", JSBool showByDefault)
        ]
    readJSON (JSObject o) = do
        let xs = fromJSObject o
        maybe (Error "failed to parse Category") Ok $ do
            JSString name <- lookup "name" xs
            JSString icon <- lookup "icon" xs
            JSBool showByDefault <- lookup "showByDefault" xs
            return $ Category
                (fromJSString name)
                (fromJSString icon)
                showByDefault
    readJSON _ = Error "failed to parse Category"


------------------------------------------------------------------------------
getFileList :: GititServerPart [FilePath]
getFileList = do
    cfg <- getConfig
    fs <- getFileStore
    files <- liftIO $ index fs
    return $ flip mapMaybe files $ \file -> do
        guard (isPageFile file)
        guard (not (isDiscussPageFile file))
        return $ repositoryPath cfg </> file


------------------------------------------------------------------------------
readCategoriesAndLocation :: FilePath -> IO ([String], Maybe (Double, Double))
readCategoriesAndLocation f = withFile f ReadMode $ \h -> handle handler $ do
    l <- hGetLine h
    if not (dashline l) then return ([], Nothing) else do
        rest <- hGetLinesTill h dotline
        let md = parseMetadata rest
        let cats = splitCategories $ maybe "" id $ lookup "categories" md
        let loc = lookup "location" md >>= \x -> readMaybe ("(" ++ x ++ ")")
        return (cats, loc)
  where
    whitespace ' ' = True
    whitespace '\t' = True
    whitespace _ = False
    dashline ('-':'-':'-':xs) | all whitespace xs = True
    dashline _ = False
    dotline ('.':'.':'.':xs) | all whitespace xs = True
    dotline _ = False
    hGetLinesTill h end = go id
      where
        go dl = do
            next <- hGetLine h
            if end next then return (dl []) else go (dl . ((next ++ "\n") ++))
    parseMetadata = either (const []) id . parse (many pMetadataLine) ""
      where
        pBlankline = try $ many (oneOf " \t") >> newline
        pMetadataLine = try $ do
            first <- letter
            rest <- many (letter <|> digit <|> oneOf "-_")
            let ident = first:rest
            skipMany (oneOf " \t")
            _ <- char ':'
            rawval <- many $ noneOf "\n\r" <|> (try $ do
                _ <- newline
                notFollowedBy pBlankline
                skipMany1 (oneOf " \t")
                return ' ')
            _ <- newline
            return (ident, trim rawval)
    splitCategories = words . map puncToSpace . trim
      where
        puncToSpace x | x `elem` ".,;:" = ' '
        puncToSpace x = x
    trim = reverse . dropWhile whitespace . reverse . dropWhile whitespace
    handler e
        | isEOFError e = return ([], Nothing)
        | otherwise = throwIO e


------------------------------------------------------------------------------
data Point = Point
    { _title :: String
    , _url :: String
    , _location :: (Double, Double)
    , _categories :: [String]
    }
  deriving (Show)


------------------------------------------------------------------------------
instance JSON Point where
    showJSON (Point title_ url (lat, lon) categories) = JSObject $ toJSObject $
        [ ("title", JSString $ toJSString title_)
        , ("location", JSObject $ toJSObject $
            [ ("lat", JSRational False (toRational lat))
            , ("lon", JSRational False (toRational lon))
            ])
        , ("url", JSString $ toJSString url)
        , ("categories", JSArray $ map (JSString . toJSString) categories)
        ]
    readJSON (JSObject o) = do
        let xs = fromJSObject o
        maybe (Error "failed to parse Point") Ok $ do
            JSString title_ <- lookup "title" xs
            JSString url <- lookup "url" xs
            JSObject l <- lookup "location" xs
            let xs' = fromJSObject l
            JSRational _ lat <- lookup "lat" xs'
            JSRational _ lon <- lookup "lon" xs'
            JSArray categories <- lookup "categories" xs
            let fromJSValue (JSString s) = Just (fromJSString s)
                fromJSValue _ = Nothing
            categories' <- mapM fromJSValue categories
            return $ Point
                (fromJSString title_)
                (fromJSString url)
                (fromRational lat, fromRational lon)
                categories'
    readJSON _ = Error "failed to parse Point"


------------------------------------------------------------------------------
getPoints :: Categories -> GititServerPart [Point]
getPoints categories = do
    cfg <- getConfig
    base <- getWikiBase
    files <- getFileList
    let pageNameFromFile = dropExtension . makeRelative (repositoryPath cfg)
    catsLocation <- liftIO $ mapM (\f -> (\x -> (pageNameFromFile f, x)) `fmap` readCategoriesAndLocation f) files
    return $ do
        (page, (cats, Just loc)) <- catsLocation
        let url = base ++ urlForPage page
        let f k = fmap (\c -> (_name c, k)) (M.lookup k categories)
        let cats' = map snd $ sort $ mapMaybe f cats
        --let cats'' = if null cats' then ["(other)"] else cats'
        return $ Point page url loc cats'


------------------------------------------------------------------------------
mapPage :: MapConfig -> Handler
mapPage mcfg = do
    base <- getWikiBase
    categories <- getCategories mcfg
    points <- getPoints categories
    let map_ = thediv !
         [ identifier "map"
         , strAttr "data-points" (toJS (showJSON points))
         ] << noHtml
    let form_ = form ! [identifier "mapform"] << fieldset <<
         [ legend << "Show on map"
         , ulist << map (toCheckbox base) (sortBy (compare `on` _name . snd) (toList categories))
         ]
    formattedPage layout $ form_ +++ map_
  where
    layout = defaultPageLayout
        { pgPageName = "Map"
        , pgShowPageTools = False
        , pgTabs = []
        , pgScripts = ["leaflet/leaflet.js", "map.js"]
        , pgTitle = "Map"
        }
    toCheckbox base (k, Category n i c) = do
        let cb = checkbox k "" ! (if c then [checked] else [])
        let uri = base ++ encString False isUnescapedInURI i
        let img = image ! [src uri, alt n, title n]
        li << [cb, label ! [thefor k] << [img, thespan << n]]
    toJS = flip showJSValue ""
