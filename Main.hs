{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Monoid
import Network.URI.Encode
import Text.Blaze.Html.Renderer.Utf8
import qualified Clay
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Link = Link
  { linkTitle :: String
  , linkUrl :: String
  , linkExtra :: String
  , linkGroup :: String
  }

groupLinks :: [Link] -> M.Map String [Link]
groupLinks = order . M.fromListWith (++) . assoc
  where
    assoc = map (\link -> (linkGroup link, [link]))
    order = M.map (sortOn linkTitle)

pointPrognosis :: [Link]
pointPrognosis =
  map ($"Alingsås") (vaderprognosen "Alingsås" ++ ballong 2515) ++
  map ($"Borlänge") (windguru 21763 ++ ballong 2435 ++ vaderprognosen "Borlänge") ++
  map ($"Dalsland") (windguru 4849 ++ ballong 2540 ++ vaderprognosen "Kroppefjäll") ++
  map ($"Gränna") (windguru 2781 ++ vaderprognosen "Gränna") ++
  map ($"Göteborg") (windguru 86 ++ vaderprognosen "Göteborg") ++
  map ($"Säve") (ballong 2512 ++ vaderprognosen "Säve" ++ windguru 254659) ++
  map ($"Landvetter") (ballong 2526 ++ vaderprognosen "Landvetter" ++ windguru 254656) ++
  map ($"Jönköping") (windguru 152 ++ ballong 2550 ++ vaderprognosen "Jönköping") ++
  map ($"Skövde") (ballong 2535 ++ vaderprognosen "Skövde" ++ windguru 32689) ++
  map ($"Såtenäs") (ballong 2520 ++ vaderprognosen "Såtenäs") ++
  map ($"Örebro") (ballong 2432 ++ vaderprognosen "Örebro" ++ windguru 27536) ++
  map ($"Skara") (vaderprognosen "Skara" ++ windguru 33291) ++
  map ($"Vara") (vaderprognosen "Vara") ++
  map ($"Uddevalla") (vaderprognosen "Uddevalla" ++ windguru 333774) ++
  map ($"Enköping") (vaderprognosen "Enköping" ++ windguru 163242) ++
  map ($"Västerås") (ballong 2446 ++ windguru 100495) ++
  map ($"Uppsala") (ballong 2458 ++ vaderprognosen "Uppsala")

  where
    vaderprognosen ident =
      [ Link "Höjdvind" height ""
      , Link "Tempkurva" temp ""
      ]
      where
        height = "http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=0"
        temp   = "http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=1"

    ballong, windguru :: Int -> [String -> Link]
    ballong ident = [Link "Ballong" ("http://www.ballong.org/drupal/vader/" <> show ident) ""]
    windguru ident = [Link "WindGuru" ("http://www.windguru.cz/int/index.php?sc=" <> show ident) ""]

fieldPrognosis :: [Link]
fieldPrognosis =
  [ Link { linkUrl = "http://www.yr.no/kart/#laga=trykk",                 linkTitle = "yr.no",         linkGroup = "Lufttryck", linkExtra = "" }

  , Link { linkUrl = "http://se.baltrad.eu",                                            linkTitle = "baltrad.eu",      linkGroup = "Radar", linkExtra = "5 min" }
  , Link { linkUrl = "http://www.radareu.cz",                                           linkTitle = "radareu.cz",      linkGroup = "Radar", linkExtra = "15 min" }
  , Link { linkUrl = "http://www.blitzortung.org/Webpages/index.php?lang=en&page_0=11", linkTitle = "blitzortung.org", linkGroup = "Blixt", linkExtra = "Live" }

  , Link { linkUrl = "http://www.smhi.se/vadret/nederbord-molnighet/radar-blixt-sverige", linkTitle = "smhi.se", linkGroup = "Radar",    linkExtra = "15 min (med blixt)" }
  , Link { linkUrl = "http://www.smhi.se/vadret/nederbord-molnighet/satellit-norden-rgb", linkTitle = "smhi.se", linkGroup = "Satellit", linkExtra = "" }

  , Link { linkUrl = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=wind10m", linkTitle = "vaderprognosen.se", linkGroup = "Vind",      linkExtra = "" }
  , Link { linkUrl = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=prec",    linkTitle = "vaderprognosen.se", linkGroup = "Nederbörd", linkExtra = "" }
  , Link { linkUrl = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=cldbase", linkTitle = "vaderprognosen.se", linkGroup = "Moln",      linkExtra = "" }

  , Link { linkUrl = "http://www.klart.se/europa/nederbörd", linkTitle = "klart.se (Europa)",  linkGroup = "Nederbörd", linkExtra = "" }
  , Link { linkUrl = "http://www.klart.se/europa/satellit",  linkTitle = "klart.se",           linkGroup = "Satellit",  linkExtra = "" }
  , Link { linkUrl = "http://www.klart.se/moln",             linkTitle = "klart.se",           linkGroup = "Moln",      linkExtra = "" }
  , Link { linkUrl = "http://www.klart.se/nederbörd",        linkTitle = "klart.se (Sverige)", linkGroup = "Nederbörd", linkExtra = "" }
  , Link { linkUrl = "http://www.klart.se/nederbörd",        linkTitle = "klart.se",           linkGroup = "Radar",     linkExtra = "60 min (med historik)" }
  , Link { linkUrl = "http://www.klart.se/vind",             linkTitle = "klart.se",           linkGroup = "Vind",      linkExtra = "" }
  , Link { linkUrl = "http://www.klart.se/lufttryck",        linkTitle = "klart.se",           linkGroup = "Lufttryck", linkExtra = "" }

  , Link { linkUrl = "http://www.vaderradar.se/gfsforecast/LTEuropeClouds", linkTitle = "vaderradar.se", linkGroup = "Moln",      linkExtra = "" }
  , Link { linkUrl = "http://www.vaderradar.se/gfsforecast/LTEuropeRain",   linkTitle = "vaderradar.se", linkGroup = "Nederbörd", linkExtra = "" }
  , Link { linkUrl = "http://www.vaderradar.se/gfsforecast/LTEuropeWind",   linkTitle = "vaderradar.se", linkGroup = "Vind",      linkExtra = "" }
  , Link { linkUrl = "http://www.vaderradar.se/gfsforecast/LTEuropeWind",   linkTitle = "vaderradar.se", linkGroup = "Lufttryck", linkExtra = "" }
  , Link { linkUrl = "http://www.vaderradar.se/radarscandinavie",           linkTitle = "vaderradar.se", linkGroup = "Radar",     linkExtra = "15 min" }
  , Link { linkUrl = "http://www.vaderradar.se/radarscanprognos",           linkTitle = "vaderradar.se", linkGroup = "Nederbörd", linkExtra = "" }

  , Link { linkUrl = "http://www.dmi.dk/vejr/til-lands/vejrkort", linkTitle = "dmi.dk", linkGroup = "Luftfuktighet", linkExtra = "" }
  , Link { linkUrl = "http://www.dmi.dk/vejr/til-lands/vejrkort", linkTitle = "dmi.dk", linkGroup = "Vind",          linkExtra = "" }
  , Link { linkUrl = "http://www.dmi.dk/vejr/til-lands/vejrkort", linkTitle = "dmi.dk", linkGroup = "Frontkarta",    linkExtra = "" }
  , Link { linkUrl = "http://www.dmi.dk/vejr/til-lands/vejrkort", linkTitle = "dmi.dk", linkGroup = "Lufttryck",     linkExtra = "" }
  ]

style :: Clay.Css
style = do
  Clay.html <> Clay.body Clay.? do
    Clay.margin Clay.nil Clay.nil Clay.nil Clay.nil
    Clay.padding Clay.nil Clay.nil Clay.nil Clay.nil

  Clay.body Clay.? do
    Clay.fontFamily ["Roboto"] [Clay.sansSerif]
    Clay.color "#757575"

  Clay.header Clay.? do
    Clay.height (Clay.rem 5)
    Clay.boxShadow Clay.nil (Clay.px 1) (Clay.px 6) (Clay.rgba 0 0 0 90)

  Clay.h1 <> Clay.h2 Clay.? do
    Clay.color "#444444"

  Clay.header Clay.** Clay.h1 Clay.? do
    Clay.margin Clay.nil Clay.nil Clay.nil Clay.nil
    Clay.lineHeight (Clay.rem 5)

  ".content" Clay.? do
    Clay.maxWidth (Clay.px 800)
    Clay.margin Clay.nil Clay.auto Clay.nil Clay.auto

  Clay.a Clay.? do
    Clay.backgroundColor "#448aff"
    Clay.color Clay.white
    Clay.textDecoration Clay.none
    Clay.fontSize (Clay.rem 0.875)

    Clay.borderRadius (Clay.px 2) (Clay.px 2) (Clay.px 2) (Clay.px 2)

    Clay.margin (Clay.px 2) (Clay.px 2) (Clay.px 2) (Clay.px 2)
    Clay.textAlign (Clay.alignSide Clay.sideCenter)
    Clay.display Clay.inlineBlock
    Clay.lineHeight (Clay.rem 3)
    Clay.width (Clay.rem 9)

compactSite :: H.Html
compactSite = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.content "width=device-width,initial-scale=1" H.! A.name "viewport"
    H.link H.! A.href "https://fonts.googleapis.com/css?family=Roboto" H.! A.rel "stylesheet" H.! A.type_ "text/css"
    H.style (H.toHtml (Clay.renderWith Clay.compact [] style))
    H.title "Flygplanering"
  H.body $ do
    H.header $ content $ H.h1 "Flight Planning Center"

    H.section $ content $ do
      H.h2 "Punktprognoser"
      H.table $ mapM_ (uncurry tableRow) (M.assocs (groupLinks pointPrognosis))

      H.h2 "Fältprognoser"
      H.table $ mapM_ (uncurry tableRow) (M.assocs (groupLinks fieldPrognosis))

      H.h2 "Metrologens Kommentarer"
      H.p $ mconcat
        [ "DMI Vecka"  `ahref` "http://www.dmi.dk/vejr/til-lands/landsudsigten"
        , "DMI Säsong" `ahref` "http://www.dmi.dk/vejr/til-lands/maaned-og-saeson"
        , "SMHI Vecka" `ahref` "http://www.smhi.se/vadret/vadret-i-sverige/meteorologens-kommentar"
        ]

      H.h2 "Flygvädret"
      H.p $ mconcat
        [ "NSWC"  `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=229"
        , "METAR" `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=300"
        , "TAF"   `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=304"
        ]

      H.p $ mconcat
        [ "LHP Område A" `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=307"
        , "LHP Område B" `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=308"
        , "LHP Område C" `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=309"
        , "LHP Område D" `ahref` "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=310"
        ]

      H.p $ mconcat
        [ "AIP SUP" `ahref` "https://aro.lfv.se/Editorial/View/IAIP?folderId=22"
        , "NOTAM"   `ahref` "https://aro.lfv.se/Links/Link/ViewLink?TorLinkId=162&type=AIS"
        ]

      H.h2 "Väderlänkar"
      H.p $ mconcat
        [ "earth.nullschool.net" `ahref` "http://earth.nullschool.net"
        , "windyty.com"          `ahref`"https://www.windyty.com"
        , "wetterzentrale.de"    `ahref` "http://www.wetterzentrale.de"
        ]

      H.h2 "Geografiska Kartor"
      H.p $ mconcat
        [ "lantmateriet.se" `ahref` "https://kso.etjanster.lantmateriet.se"
        , "hitta.se"        `ahref` "http://www.hitta.se/kartan"
        , "eniro.se"        `ahref` "http://kartor.eniro.se"
        , "bing.com"        `ahref` "https://www.bing.com/maps"
        , "google.com"      `ahref` "https://www.google.com/maps"
        ]
  where
    content = H.div H.! A.class_ "content"

    ahref :: String -> String -> H.Html
    ahref a b = H.a (H.toHtml a) H.! A.href (H.toValue (urlenc b))

    fromLink :: Link -> H.Html
    fromLink link = linkTitle link `ahref` linkUrl link H.! title
      where
        title = A.title (H.toValue (linkTitle link))

    tableRow heading links = H.tr $ do
      H.td (H.toHtml heading)
      H.td (mconcat $ map fromLink links)

urlenc :: String -> String
urlenc = encodeWith escape
  where
    escape c = isAllowed c ||
      c == '/' ||
      c == ':' ||
      c == '?' ||
      c == '&' ||
      c == '#' ||
      c == '='

main :: IO ()
main = renderHtmlToByteStringIO B.putStrLn compactSite
