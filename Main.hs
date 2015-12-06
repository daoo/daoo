{-# LANGUAGE OverloadedStrings #-}

import Data.List hiding (group)
import Data.Monoid
import Network.URI.Encode
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Link = Link
  { title :: String
  , url :: String
  , extra :: String
  , group :: String
  }

vaderprognosen :: String -> [String -> Link]
vaderprognosen ident =
  [ Link "Höjdvind" height ""
  , Link "Temperaturkurva" temp ""
  ]
  where
    height = "http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=0"
    temp   = "http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=1"

ballong :: Int -> [String -> Link]
ballong ident = [Link "Ballongväder" ("http://www.ballong.org/drupal/vader/" <> show ident) ""]

windguru :: Int -> [String -> Link]
windguru ident = [Link "Översiktsprognos" ("http://www.windguru.cz/int/index.php?sc=" <> show ident) ""]

pointPrognosis :: [Link]
pointPrognosis =
  map ($"Alingsås") (vaderprognosen "Alingsås" ++ ballong 2515) ++
  map ($"Borlänge") (windguru 21763 ++ ballong 2435 ++ vaderprognosen "Borlänge") ++
  map ($"Dalsland") (windguru 4849 ++ ballong 2540 ++ vaderprognosen "Kroppefjäll") ++
  map ($"Gränna") (windguru 2781 ++ vaderprognosen "Gränna") ++
  map ($"Göteborg") (windguru 86 ++ vaderprognosen "Göteborg") ++
  map ($"Säve") (ballong 2512) ++
  map ($"Landvetter") (ballong 2526) ++
  map ($"Jönköping") (windguru 152 ++ ballong 2550 ++ vaderprognosen "Jönköping") ++
  map ($"Skövde") (ballong 2535 ++ vaderprognosen "Skövde" ++ windguru 32689) ++
  map ($"Såtenäs") (ballong 2520 ++ vaderprognosen "Såtenäs") ++
  map ($"Örebro") (ballong 2432 ++ vaderprognosen "Örebro" ++ windguru 27536) ++
  map ($"Skara") (vaderprognosen "Skara" ++ windguru 33291) ++
  map ($"Vara") (vaderprognosen "Vara") ++
  map ($"Uddevalla") (vaderprognosen "Uddevalla" ++ windguru 333774)

fieldPrognosis :: [Link]
fieldPrognosis =
  [ Link { url = "http://www.klart.se/lufttryck",                     title = "Europa",       group = "Lufttryck", extra = "" }
  , Link { url = "http://www.yr.no/kart/#laga=trykk",                 title = "Skandinavien", group = "Lufttryck", extra = "" }
  , Link { url = "http://www.vaderradar.se/gfsforecast/LTEuropeWind", title = "Europa",       group = "Lufttryck", extra = "" }

  , Link { url = "http://se.baltrad.eu",                                            title = "Östersjön",    group = "Radar", extra = "5 min" }
  , Link { url = "http://www.radareu.cz",                                           title = "Europa",       group = "Radar", extra = "15 min" }
  , Link { url = "http://www.vaderradar.se/radarscandinavie",                       title = "Norra Europa", group = "Radar", extra = "15 min" }
  , Link { url = "http://www.blitzortung.org/Webpages/index.php?lang=en&page_0=11", title = "Europa",       group = "Blixt", extra = "Live" }

  , Link { url = "http://www.smhi.se/vadret/nederbord-molnighet/radar-blixt-sverige", title = "Sverige", group = "Radar", extra = "15 min (med blixt)" }
  , Link { url = "http://www.smhi.se/vadret/nederbord-molnighet/satellit-norden-rgb", title = "Norden",  group = "Satellit", extra = "" }

  , Link { url = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=wind10m", title = "Södra Sverige", group = "Vind", extra = "" }
  , Link { url = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=prec",    title = "Södra Sverige", group = "Nederbörd", extra = "" }
  , Link { url = "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=cldbase", title = "Södra Sverige", group = "Moln", extra = "" }

  , Link { url = "http://www.klart.se/europa/nederbörd", title = "Europa",  group = "Nederbörd", extra = "" }
  , Link { url = "http://www.klart.se/europa/satellit",  title = "Europa",  group = "Satellit", extra = "" }
  , Link { url = "http://www.klart.se/moln",             title = "Sverige", group = "Moln", extra = "" }
  , Link { url = "http://www.klart.se/nederb%C3%B6rd",   title = "Sverige", group = "Nederbörd", extra = "" }
  , Link { url = "http://www.klart.se/nederbörd",        title = "Sverige", group = "Radar", extra = "60 min (med historik)" }
  , Link { url = "http://www.klart.se/vind",             title = "Sverige", group = "Vind", extra = "" }

  , Link { url = "http://www.vaderradar.se/gfsforecast/LTEuropeClouds", title = "Europa",  group = "Moln", extra = "" }
  , Link { url = "http://www.vaderradar.se/gfsforecast/LTEuropeRain",   title = "Europa",  group = "Nederbörd", extra = "" }
  , Link { url = "http://www.vaderradar.se/gfsforecast/LTEuropeWind",   title = "Europa",  group = "Vind", extra = "" }
  , Link { url = "http://www.vaderradar.se/radarscanprognos",           title = "Sverige", group = "Nederbörd", extra = "" }

  , Link { url = "http://www.dmi.dk/vejr/til-lands/vejrkort", title = "Danmark", group = "Luftfuktighet", extra = "" }
  , Link { url = "http://www.dmi.dk/vejr/til-lands/vejrkort", title = "Danmark", group = "Vind", extra = "" }
  , Link { url = "http://www.dmi.dk/vejr/til-lands/vejrkort", title = "Europa",  group = "Frontkarta", extra = "" }
  , Link { url = "http://www.dmi.dk/vejr/til-lands/vejrkort", title = "Danmark", group = "Lufttryck", extra = "" }

  , Link { url = "http://www.dmi.dk/vejr/til-lands/landsudsigten",                                           title = "Danmark", group = "Väderöversikt", extra = "" }
  , Link { url = "http://www.dmi.dk/vejr/til-lands/maaned-og-saeson",                                        title = "Danmark", group = "Månad och Säsong", extra = "" }
  , Link { url = "http://www.smhi.se/vadret/vadret-i-sverige/vaderoversikt-sverige-meteorologens-kommentar", title = "Sverige", group = "Väderöversikt", extra = "" }
  ]

main :: IO ()
main = renderHtmlToByteStringIO B.putStrLn $ H.docTypeHtml $ do
  H.head $ do
    H.meta H.! A.charset "utf-8"
    H.meta H.! A.content "width=device-width,initial-scale=1" H.! A.name "viewport"
    H.title "Flygplanering"
  H.body $ do
    H.h1 "Punktprognoser"
    H.table $ mapM_ (uncurry tableRow) (M.assocs (groupLinks pointPrognosis))

    H.h1 "Fältprognoser"
    H.table $ mapM_ (uncurry tableRow) (M.assocs (groupLinks fieldPrognosis))

    H.h1 "Flygvädret"
    H.p $ commasep
      [ H.a "NSWC"    H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=229"
      , H.a "METAR"   H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=300"
      , H.a "TAF"     H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=304"
      ]

    H.p $ commasep
      [ H.a "LHP Område A" H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=307"
      , H.a "LHP Område B" H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=308"
      , H.a "LHP Område C" H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=309"
      , H.a "LHP Område D" H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=310"
      ]

    H.p $ commasep
      [ H.a "AIP SUP" H.! A.href "https://aro.lfv.se/Editorial/View/IAIP?folderId=22"
      , H.a "NOTAM"   H.! A.href "https://aro.lfv.se/Links/Link/ViewLink?TorLinkId=162&type=AIS"
      ]

    H.h1 "Väderlänkar"
    H.p $ commasep
      [ H.a "Interaktiv väderkarta" H.! A.href "http://earth.nullschool.net"
      , H.a "Interaktiv väderkarta" H.! A.href"https://www.windyty.com"
      , H.a "Nördväder" H.! A.href "http://www.wetterzentrale.de"
      ]

    H.h1 "Geografiska Kartor"
    H.p $ commasep
      [ H.a "Lantmäteriet" H.! A.href "http://kso2.lantmateriet.se"
      , H.a "Hitta.se"     H.! A.href "http://hitta.se/kartan"
      , H.a "Eniro"        H.! A.href "http://kartor.eniro.se"
      , H.a "Bing"         H.! A.href "https://www.bing.com/maps"
      , H.a "Google"       H.! A.href "https://google.com/maps"
      ]
  where
    ahref :: (String, String, String) -> H.Html
    ahref (u, inner, desc) = H.a (H.toHtml inner) H.! A.href (H.toValue u) H.! A.title (H.toValue desc)

    commasep = mconcat . intersperse ", "

    tableRow heading links = H.tr $ do
      H.td (H.toHtml heading)
      H.td (commasep $ map (ahref . fromLink) links)

    fromLink :: Link -> (String, String, String)
    fromLink link = (encodeWith escape (url link), title link, extra link)

    escape c = isAllowed c || c == '/' || c == ':' || c == '?' || c == '&' || c == '#' || c == '='

groupLinks :: [Link] -> M.Map String [Link]
groupLinks = order . M.fromListWith (++) . assoc
  where
    assoc = map (\link -> (group link, [link]))
    order = M.map (sortOn title)
