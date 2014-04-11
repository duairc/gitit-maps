module Network.Gitit.Plugins.Maps.Preprocess (plugin) where

-- base ----------------------------------------------------------------------
import           Control.Monad (msum)
import           Data.Functor ((<$>))
import           Data.List (unfoldr)
import           Numeric (showFFloat)
import           Text.Read (readMaybe)


-- cgi -----------------------------------------------------------------------
import          Network.CGI (formDecode)


-- gitit ---------------------------------------------------------------------
import           Network.Gitit (Plugin (PreCommitTransform))


-- HTTP ----------------------------------------------------------------------
import           Network.HTTP (urlDecode)


-- network -------------------------------------------------------------------
import           Network.URI (URI (URI), parseURIReference)


-- xhtml ---------------------------------------------------------------------
import           Text.XHtml.Strict (stringToHtmlString)


------------------------------------------------------------------------------
plugin :: Plugin
plugin = PreCommitTransform (return . mapsPre)


------------------------------------------------------------------------------
mapsPre :: String -> String
mapsPre [] = []
mapsPre xs@('{':'{':'m':'a':'p':' ':rest) = case breakOnDoubleBrace rest of
    Nothing -> "fuck"
    Just (url, rest') -> maybe (continue url rest') (++ mapsPre rest') $ do
        (URI _ _ _ ('?':query) _) <- parseURIReference url
        let params = formDecode query
        if ("layer", "c") `elem` params
            then streetView params
            else onlyMap params
  where
    continue url rest' = concat ["{{map ", url, "}}", mapsPre rest']
    streetView params = do
        streetView <- makeStreetView params
        ll <- lookup "cbll" params
        let params' = [("ll", ll)]
        map_ <- makeMap params'
        satellite <- makeSatellite params'
        return $ makeFigure [map_, satellite, streetView] (getCaption params)
    onlyMap params = do
        map_ <- makeMap params
        satellite <- makeSatellite params
        return $ makeFigure [map_, satellite] (getCaption params)
    getCaption params = urlDecode . maybe "" id $ msum
        [ lookup "hnear" params
        , lookup "q" params
        ]
    makeFigure contents caption = concat
        [ "<figure>"
        , concatMap ("\n  " ++) contents
        , "\n\n  <figcaption>"
        , stringToHtmlString caption
        , "</figcaption>"
        , "\n</figure>"
        ]
    makeStreetView params = do
        (latitude, longitude) <- lookup "cbll" params >>= readLocation
        splitOn ',' <$> lookup "cbp" params
        [_, h, _, z, p] <- splitOn ',' <$> lookup "cbp" params
        heading <- readMaybe h :: Maybe Double
        let zoom = case z of
             "1" -> 7.0
             "2" -> 9.0
             _ -> 4.0
        pitch <- negate <$> readMaybe p :: Maybe Double
        return $ concat
            [ "<streetview location=\""
            , show latitude
            , ","
            , show longitude
            , "\" zoom=\""
            , show zoom
            , "\" heading=\""
            , show heading
            , "\" pitch=\""
            , show pitch
            , "\" size=\"full\" />"
            ]
    makeMap = makeMapMeta "map"
    makeSatellite = makeMapMeta "satellite"
    makeMapMeta meta params = do
        (latitude, longitude) <- lookup "ll" params >>= readLocation
        let z = maybe 15.0 id $ lookup "z" params >>= readMaybe
        let zoom = if z < 3 then 0 else (z - 3) / 1.8
        return $ concat
            [ "<"
            , meta
            , " location=\""
            , show latitude
            , ","
            , show longitude
            , "\" zoom=\""
            , showFFloat (Just 1) zoom ""
            , "\" size=\"half\" />"
            ]
    readLocation :: String -> Maybe (Double, Double)
    readLocation string = readMaybe ("(" ++ string ++ ")")
    splitOn = unfoldr . go
      where
        go _ [] = Nothing
        go x xs = let (h, t) = span (/= x) xs in Just (h, drop 1 t)
    breakOnDoubleBrace = go id
      where
        go dlist [] = Nothing
        go dlist ('}':'}':xs) = Just (dlist [], xs)
        go dlist ('–':xs) = go (dlist . ('-':)) xs
        go dlist (x:xs) = go (dlist . (x:)) xs
mapsPre (x:xs) = x : mapsPre xs
