{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Clay hiding (map)
import Data.Monoid
import Site
import Text.Blaze.Html.Renderer.Utf8
import qualified Clay.Media as Media
import qualified Data.ByteString.Char8 as B

pointPrognosis :: Html
pointPrognosis = do
  mkLevel2 "Alingsås" $ do
    ballong 2515
    vaderprognosen "Alingsås"
  mkLevel2 "Borlänge" $ do
    ballong 2435
    vaderprognosen "Borlänge"
    windguru 21763
  mkLevel2 "Dalsland" $ do
    ballong 2540
    vaderprognosen "Kroppefjäll"
    windguru 4849
  mkLevel2 "Gränna" $ do
    vaderprognosen "Gränna"
    windguru 2781
  mkLevel2 "Göteborg" $ do
    vaderprognosen "Göteborg"
    windguru 86
  mkLevel2 "Säve" $ do
    ballong 2512
  mkLevel2 "Landvetter" $ do
    ballong 2526
  mkLevel2 "Jönköping" $ do
    ballong 2550
    vaderprognosen "Jönköping"
    windguru 152
  mkLevel2 "Skövde" $ do
    ballong 2535
    vaderprognosen "Skövde"
    windguru 32689
  mkLevel2 "Såtenäs" $ do
    ballong 2520
    vaderprognosen "Såtenäs"
  mkLevel2 "Örebro" $ do
    ballong 2432
    vaderprognosen "Örebro"
    windguru 27536
  mkLevel2 "Skara" $ do
    vaderprognosen "Skara"
    windguru 33291
  mkLevel2 "Vara" $ do
    vaderprognosen "Vara"
  mkLevel2 "Uddevalla" $ do
    vaderprognosen "Uddevalla"
    windguru 333774

  where
    vaderprognosen ident = do
      mkButton1 "Höjdvind" ("http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=0")
      mkButton1 "Tempkurva" ("http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=1")

    ballong, windguru :: Int -> Html
    ballong ident = mkButton1 "Ballongväder" ("http://www.ballong.org/drupal/vader/" <> show ident)
    windguru ident = mkButton1 "Översiktsprognos" ("http://www.windguru.cz/int/index.php?sc=" <> show ident)

fieldPrognosis :: Html
fieldPrognosis = do
  mkLevel2 "Blixt" $ do
    mkButton "blitzortung.org" "http://www.blitzortung.org/Webpages/index.php?lang=en&page_0=11" "Live"

  mkLevel2 "Frontkarta" $ do
    mkButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

  mkLevel2 "Lufttryck" $ do
    mkButton1 "klart.se" "http://www.klart.se/lufttryck"
    mkButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"
    mkButton1 "yr.no" "http://www.yr.no/kart/#laga=trykk"
    mkButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeWind"

  mkLevel2 "Luftfuktighet" $ do
    mkButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

  mkLevel2 "Moln" $ do
    mkButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=cldbase"
    mkButton1 "klart.se" "http://www.klart.se/moln"
    mkButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeClouds"

  mkLevel2 "Nederbörd" $ do
    mkButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=prec"
    mkButton1 "klart.se (Europa)" "http://www.klart.se/europa/nederbörd"
    mkButton1 "klart.se (Sverige)" "http://www.klart.se/nederbörd"
    mkButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeRain"
    mkButton1 "vaderradar.se" "http://www.vaderradar.se/radarscanprognos"

  mkLevel2 "Radar" $ do
    mkButton "baltrad.eu" "http://se.baltrad.eu" "5 min"
    mkButton "radareu.cz" "http://www.radareu.cz" "15 min"
    mkButton "smhi.se" "http://www.smhi.se/vadret/nederbord-molnighet/radar-blixt-sverige" "15 min (med blixt)"
    mkButton "klart.se" "http://www.klart.se/nederbörd" "60 min (med historik)"
    mkButton "vaderradar.se" "http://www.vaderradar.se/radarscandinavie" "15 min"

  mkLevel2 "Satellit" $ do
    mkButton1 "smhi.se" "http://www.smhi.se/vadret/nederbord-molnighet/satellit-norden-rgb"
    mkButton1 "klart.se" "http://www.klart.se/europa/satellit"

  mkLevel2 "Vind" $ do
    mkButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=wind10m"
    mkButton1 "klart.se" "http://www.klart.se/vind"
    mkButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeWind"
    mkButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

compactSite :: Html
compactSite = docTypeHtml $ do
  mkHead "Flygplanering" (renderWith compact [] siteStyle)
  mkBody $ do
    mkHeader "Flight Planning Center"

    mkContent $ do
      mkLevel1 "Punktprognoser"
        pointPrognosis

      mkLevel1 "Fältprognoser"
        fieldPrognosis

      mkLevel1 "Metrologens Kommentarer" $ do
        mkButton1 "DMI Vecka"  "http://www.dmi.dk/vejr/til-lands/landsudsigten"
        mkButton1 "DMI Säsong" "http://www.dmi.dk/vejr/til-lands/maaned-og-saeson"
        mkButton1 "SMHI Vecka" "http://www.smhi.se/vadret/vadret-i-sverige/meteorologens-kommentar"

      mkLevel1 "Flygvädret" $ do
        mkButton1 "NSWC"  "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=229"
        mkButton1 "METAR" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=300"
        mkButton1 "TAF"   "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=304"

        mkButton1 "LHP Område A" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=307"
        mkButton1 "LHP Område B" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=308"
        mkButton1 "LHP Område C" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=309"
        mkButton1 "LHP Område D" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=310"

        mkButton1 "AIP SUP" "https://aro.lfv.se/Editorial/View/IAIP?folderId=22"
        mkButton1 "NOTAM"   "https://aro.lfv.se/Links/Link/ViewLink?TorLinkId=162&type=AIS"

      mkLevel1 "Väderlänkar" $ do
        mkButton1 "earth.nullschool.net" "http://earth.nullschool.net"
        mkButton1 "windyty.com"          "https://www.windyty.com"
        mkButton1 "wetterzentrale.de"    "http://www.wetterzentrale.de"
        mkButton1 "svn.universeum.se"    "http://svn.universeum.se/index_vadretnu.htm"

      mkLevel1 "Geografiska Kartor" $ do
        mkButton1 "lantmateriet.se" "https://kso.etjanster.lantmateriet.se"
        mkButton1 "hitta.se"        "http://www.hitta.se/kartan"
        mkButton1 "eniro.se"        "http://kartor.eniro.se"
        mkButton1 "bing.com"        "https://www.bing.com/maps"
        mkButton1 "google.com"      "https://www.google.com/maps"

siteStyle :: Css
siteStyle = do
  html <> body ? do
    margin nil nil nil nil
    padding nil nil nil nil

  body ? do
    fontFamily [] [sansSerif]
    color medium

  header ? do
    backgroundColor dark
    color light
    fontSize (px 28)
    fontWeight bold

    -- boxShadowsWithSpread shadow

    wide $ paddingLeft (px 60)
    narrow <> narrower $ textAlign (alignSide sideCenter)
    height (px 64)
    lineHeight (px 64)

  h1 <> h2 ? do
    color medium
    marginBottom nil

  ".content" ? do
    maxWidth wideWidth
    marginLeft auto
    marginRight auto

  a ? do
    backgroundColor dark
    color light
    textDecoration none
    fontSize (px 14)
    fontWeight (weight 500)

    -- borderRadius (px 2) (px 2) (px 2) (px 2)
    -- boxShadowsWithSpread shadow

    textAlign (alignSide sideCenter)
    lineHeight (px 36)

    display inlineBlock
    margin (pct 1) (pct 0.6) (pct 1) (pct 0.6)
    height (px 36)
    wide $ width (pct 23.8)
    narrow $ width (pct 48.8)
    narrower $ width (pct 98.8)

  where
    dark = "#2196f3"
    medium = "#212121"
    light = "#ffffff"

    wideWidth = px 800
    narrowerWidth = px 400

    wide = query Clay.all [Media.minWidth wideWidth]
    narrow = query Clay.all [Media.minWidth narrowerWidth, Media.maxWidth wideWidth]
    narrower = query Clay.all [Media.maxWidth narrowerWidth]

main :: IO ()
main = renderHtmlToByteStringIO B.putStrLn compactSite
