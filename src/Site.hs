{-# LANGUAGE OverloadedStrings #-}
module Site
  ( Html
  , docTypeHtml
  , mkBody
  , mkButton
  , mkButton1
  , mkContent
  , mkHead
  , mkHeader
  , mkRow
  , mkSubHeader
  , mkTable
  , mkTableButton
  , mkTableButton1
  , mkTableRow
  ) where

import Data.Text.Lazy (Text)
import Network.URI.Encode
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Html = H.Html

docTypeHtml :: H.Html -> H.Html
docTypeHtml = H.docTypeHtml

mkHead :: String -> Text -> H.Html
mkHead title style = H.head $ do
  H.meta ! A.charset "utf-8"
  H.meta ! A.content "width=device-width,initial-scale=1" ! A.name "viewport"
  H.style (H.toHtml style)
  H.title (H.toHtml title)

mkBody :: H.Html -> H.Html
mkBody = H.body

mkHeader :: String -> H.Html
mkHeader = H.header . H.h1 . H.toHtml

mkSubHeader :: String -> H.Html
mkSubHeader = H.h2 . H.toHtml

mkContent :: H.Html -> H.Html
mkContent = H.div ! A.class_ "content"

mkButton :: String -> String -> String -> H.Html
mkButton label url description = a ! href ! title
  where
    a = H.a (H.toHtml label)
    href = A.href (H.toValue (urlenc url))
    title = A.title (H.toValue description)

mkButton1 :: String -> String -> H.Html
mkButton1 label url = a ! href
  where
    a = H.a (H.toHtml label)
    href = A.href (H.toValue (urlenc url))

mkRow :: H.Html -> H.Html
mkRow = H.div

mkTableRow :: String -> H.Html -> H.Html
mkTableRow heading cells = H.tr $ do
  H.th (H.toHtml heading)
  cells

mkTableCell :: H.Html -> H.Html
mkTableCell = H.td

mkTable :: H.Html -> H.Html
mkTable = H.table

mkTableButton :: String -> String -> String -> Html
mkTableButton label url desc = mkTableCell (mkButton label url desc)

mkTableButton1 :: String -> String -> Html
mkTableButton1 label url = mkTableCell (mkButton1 label url)

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
