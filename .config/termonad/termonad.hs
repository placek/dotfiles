module Main where

import Data.Maybe (fromMaybe)

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
  , FontConfig, FontSize(FontSizePoints), defaultFontConfig, fontConfig
  , fontFamily, fontSize
  , start
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, List8, Palette(ExtendedPalette), addColourExtension
  , createColour, createColourExtension, cursorBgColour, defaultColourConfig
  , defaultLightColours, foregroundColour, palette, mkList8, unsafeMkList8
  )

myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOff
          , fontConfig = fontConf
          }
    }

fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Iosevka"
    , fontSize = FontSizePoints 12
    }

myColourConfig :: ColourConfig (AlphaColour Double)
myColourConfig =
  defaultColourConfig
    -- normal colour of the cursor
    { cursorBgColour = Set (createColour 245 245 245)
    -- foreground colour of text of the terminal
    , foregroundColour = Set (createColour 245 245 245)
    -- extended palette that has 8 colours standard colors and then 8 light colors.
    , palette = ExtendedPalette myStandardColours
                                (fromMaybe defaultLightColours myLightColours)
    }
  where
    myStandardColours :: List8 (AlphaColour Double)
    myStandardColours = unsafeMkList8
      [ createColour  44  62  80 -- background
      , createColour 231  76  60 -- red
      , createColour  46 204 113 -- green
      , createColour 241 196  15 -- yellow
      , createColour 155  89 182 -- magenta
      , createColour 180  30 120 -- bright pink
      , createColour  26 188 156 -- cyan
      , createColour 245 245 245 -- white
      ]

    myLightColours :: Maybe (List8 (AlphaColour Double))
    myLightColours = mkList8
        [ createColour  52  73  94 -- light black
        , createColour 231  76  60 -- red
        , createColour  46 204 113 -- light green
        , createColour 241 196  15 -- yellow
        , createColour 155  89 182 -- magenta
        , createColour 140  30 80  -- dark pink
        , createColour  26 188 156 -- cyan
        , createColour 245 245 245 -- white
        ]

main :: IO ()
main = do
  myColourExt <- createColourExtension myColourConfig
  let newTMConfig = addColourExtension myTMConfig myColourExt
  start newTMConfig
