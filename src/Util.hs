module Util
  ( showText
  ) where

import Data.Text (pack, Text)

showText :: (Show a) => a -> Text
showText = pack . show

