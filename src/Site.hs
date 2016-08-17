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
  , mkSubHeader
  , mkLevel1
  , mkLevel2
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
mkHeader = H.header . H.toHtml

mkSubHeader :: String -> H.Html
mkSubHeader = H.h1 . H.toHtml

mkSubSubHeader :: String -> H.Html
mkSubSubHeader = H.h1 . H.toHtml

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

mkLevel1 :: String -> H.Html -> H.Html
mkLevel1 heading inner = H.div $ do
  H.h1 (H.toHtml heading)
  inner

mkLevel2 :: String -> H.Html -> H.Html
mkLevel2 heading inner = H.div $ do
  H.h2 (H.toHtml heading)
  inner

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
