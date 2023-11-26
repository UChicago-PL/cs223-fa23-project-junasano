{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cs223_fa23_project_junasano (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cs223_fa23_project_junasano"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Fractal generator in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
