module Main where

import Data.Maybe (fromMaybe)

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
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
          }
    }

myColourConfig :: ColourConfig (AlphaColour Double)
myColourConfig =
  defaultColourConfig
    -- normal colour of the cursor
    { cursorBgColour = Set (createColour 120 80 110)
    -- foreground colour of text of the terminal
    , foregroundColour = Set (createColour 220 180 210)
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
      , createColour 180 160 120 -- light brown
      ]

    myLightColours :: Maybe (List8 (AlphaColour Double))
    myLightColours = mkList8
        [ createColour  70  60  50 -- brown
        , createColour 220  30  20 -- light red
        , createColour  40 210  20 -- light green
        , createColour 220 200  20 -- yellow
        , createColour  40  30 180 -- purple
        , createColour 140  30 80  -- dark pink
        , createColour  50 200 160 -- light teal
        , createColour 220 200 150 -- light brown
        ]

main :: IO ()
main = do
  myColourExt <- createColourExtension myColourConfig
  let newTMConfig = addColourExtension myTMConfig myColourExt
  start newTMConfig
