{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid
import Site
import Text.Blaze.Html.Renderer.Utf8
import qualified Clay
import qualified Data.ByteString.Char8 as B

pointPrognosis :: Html
pointPrognosis = mkTable $ do
  mkTableRow "Alingsås" $ do
    ballong 2515
    vaderprognosen "Alingsås"
  mkTableRow "Borlänge" $ do
    ballong 2435
    vaderprognosen "Borlänge"
    windguru 21763
  mkTableRow "Dalsland" $ do
    ballong 2540
    vaderprognosen "Kroppefjäll"
    windguru 4849
  mkTableRow "Gränna" $ do
    vaderprognosen "Gränna"
    windguru 2781
  mkTableRow "Göteborg" $ do
    windguru 86
    vaderprognosen "Göteborg"
  mkTableRow "Säve" $ do
    ballong 2512
  mkTableRow "Landvetter" $ do
    ballong 2526
  mkTableRow "Jönköping" $ do
    ballong 2550
    vaderprognosen "Jönköping"
    windguru 152
  mkTableRow "Skövde" $ do
    ballong 2535
    vaderprognosen "Skövde"
    windguru 32689
  mkTableRow "Såtenäs" $ do
    ballong 2520
    vaderprognosen "Såtenäs"
  mkTableRow "Örebro" $ do
    ballong 2432
    vaderprognosen "Örebro"
    windguru 27536
  mkTableRow "Skara" $ do
    vaderprognosen "Skara"
    windguru 33291
  mkTableRow "Vara" $ do
    vaderprognosen "Vara"
  mkTableRow "Uddevalla" $ do
    vaderprognosen "Uddevalla"
    windguru 333774

  where
    vaderprognosen ident = do
      mkTableButton1 "Höjdvind" ("http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=0")
      mkTableButton1 "Tempkurva" ("http://www.vaderprognosen.se/vader/prognos/hpfcst.php?place=" <> ident <> "&type=1")

    ballong, windguru :: Int -> Html
    ballong ident = mkTableButton1 "Ballongväder" ("http://www.ballong.org/drupal/vader/" <> show ident)
    windguru ident = mkTableButton1 "Översiktsprognos" ("http://www.windguru.cz/int/index.php?sc=" <> show ident)

fieldPrognosis :: Html
fieldPrognosis = mkTable $ do
  mkTableRow "Blixt" $ do
    mkTableButton "blitzortung.org" "http://www.blitzortung.org/Webpages/index.php?lang=en&page_0=11" "Live"

  mkTableRow "Frontkarta" $ do
    mkTableButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

  mkTableRow "Lufttryck" $ do
    mkTableButton1 "klart.se" "http://www.klart.se/lufttryck"
    mkTableButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"
    mkTableButton1 "yr.no" "http://www.yr.no/kart/#laga=trykk"
    mkTableButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeWind"

  mkTableRow "Luftfuktighet" $ do
    mkTableButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

  mkTableRow "Moln" $ do
    mkTableButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=cldbase"
    mkTableButton1 "klart.se" "http://www.klart.se/moln"
    mkTableButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeClouds"

  mkTableRow "Nederbörd" $ do
    mkTableButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=prec"
    mkTableButton1 "klart.se (Europa)" "http://www.klart.se/europa/nederbörd"
    mkTableButton1 "klart.se (Sverige)" "http://www.klart.se/nederbörd"
    mkTableButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeRain"
    mkTableButton1 "vaderradar.se" "http://www.vaderradar.se/radarscanprognos"

  mkTableRow "Radar" $ do
    mkTableButton "baltrad.eu" "http://se.baltrad.eu" "5 min"
    mkTableButton "radareu.cz" "http://www.radareu.cz" "15 min"
    mkTableButton "smhi.se" "http://www.smhi.se/vadret/nederbord-molnighet/radar-blixt-sverige" "15 min (med blixt)"
    mkTableButton "klart.se" "http://www.klart.se/nederbörd" "60 min (med historik)"
    mkTableButton "vaderradar.se" "http://www.vaderradar.se/radarscandinavie" "15 min"

  mkTableRow "Satellit" $ do
    mkTableButton1 "smhi.se" "http://www.smhi.se/vadret/nederbord-molnighet/satellit-norden-rgb"
    mkTableButton1 "klart.se" "http://www.klart.se/europa/satellit"

  mkTableRow "Vind" $ do
    mkTableButton1 "vaderprognosen.se" "http://www.vaderprognosen.se/vader/prognos/index.php?d=2&a=2&aoi=th1&p=wind10m"
    mkTableButton1 "klart.se" "http://www.klart.se/vind"
    mkTableButton1 "vaderradar.se" "http://www.vaderradar.se/gfsforecast/LTEuropeWind"
    mkTableButton1 "dmi.dk" "http://www.dmi.dk/vejr/til-lands/vejrkort"

compactSite :: Html
compactSite = docTypeHtml $ do
  mkHead "Flygplanering" (Clay.renderWith Clay.compact [] style)
  mkBody $ do
    mkHeader "Flight Planning Center"

    mkContent $ do
      mkSubHeader "Punktprognoser"
      pointPrognosis

      mkSubHeader "Fältprognoser"
      fieldPrognosis

      mkSubHeader "Metrologens Kommentarer"
      mkRow $ do
        mkButton1 "DMI Vecka"  "http://www.dmi.dk/vejr/til-lands/landsudsigten"
        mkButton1 "DMI Säsong" "http://www.dmi.dk/vejr/til-lands/maaned-og-saeson"
        mkButton1 "SMHI Vecka" "http://www.smhi.se/vadret/vadret-i-sverige/meteorologens-kommentar"

      mkSubHeader "Flygvädret"
      mkRow $ do
        mkButton1 "NSWC"  "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=229"
        mkButton1 "METAR" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=300"
        mkButton1 "TAF"   "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=304"

      mkRow $ do
        mkButton1 "LHP Område A" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=307"
        mkButton1 "LHP Område B" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=308"
        mkButton1 "LHP Område C" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=309"
        mkButton1 "LHP Område D" "https://aro.lfv.se/Links/Link/ViewLink?type=MET&TorLinkId=310"

      mkRow $ do
        mkButton1 "AIP SUP" "https://aro.lfv.se/Editorial/View/IAIP?folderId=22"
        mkButton1 "NOTAM"   "https://aro.lfv.se/Links/Link/ViewLink?TorLinkId=162&type=AIS"

      mkSubHeader "Väderlänkar"
      mkRow $ do
        mkButton1 "earth.nullschool.net" "http://earth.nullschool.net"
        mkButton1 "windyty.com"          "https://www.windyty.com"
        mkButton1 "wetterzentrale.de"    "http://www.wetterzentrale.de"
        mkButton1 "svn.universeum.se"    "http://svn.universeum.se/index_vadretnu.htm"

      mkSubHeader "Geografiska Kartor"
      mkRow $ do
        mkButton1 "lantmateriet.se" "https://kso.etjanster.lantmateriet.se"
        mkButton1 "hitta.se"        "http://www.hitta.se/kartan"
        mkButton1 "eniro.se"        "http://kartor.eniro.se"
        mkButton1 "bing.com"        "https://www.bing.com/maps"
        mkButton1 "google.com"      "https://www.google.com/maps"

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

main :: IO ()
main = renderHtmlToByteStringIO B.putStrLn compactSite
