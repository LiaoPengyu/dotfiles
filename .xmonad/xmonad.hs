-- Base
import           System.Exit                         (exitSuccess)
import           XMonad
import           XMonad.Operations                   (restart)
import qualified XMonad.StackSet                     as W

-- Actions
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.CycleWS              (WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search               as S
import           XMonad.Actions.WindowGo             (runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)

-- Data
import           Data.Char                           (isSpace, toUpper)
import           Data.List
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust, isJust,
                                                      maybeToList)
import           Data.Monoid

-- Hooks
import           XMonad.Hooks.DynamicLog             (PP (..), dynamicLogWithPP,
                                                      shorten, wrap,
                                                      xmobarColor, xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks            (ToggleStruts (..),
                                                      avoidStruts, docks,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      isFullscreen)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WorkspaceHistory

import           XMonad.Config.Desktop
-- Layouts
import           XMonad.Layout.BinarySpacePartition  as BSP
import           XMonad.Layout.Decoration
import           XMonad.Layout.Gaps
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

-- Layouts modifiers
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.Magnifier
import           XMonad.Layout.MultiToggle           (EOT (EOT), mkToggle,
                                                      single, (??))
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation

-- Prompt
import           Control.Arrow                       (first)
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Input
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.Unicode
import           XMonad.Prompt.XMonad

-- Utilities
import           XMonad.Util.EZConfig                (additionalKeysP,
                                                      checkKeymap, mkKeymap)
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runProcessWithInput,
                                                      safeSpawn, spawnPipe)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare        (getSortByIndex,
                                                      getSortByTag,
                                                      getSortByXineramaPhysicalRule,
                                                      getSortByXineramaRule)

import           Control.Monad                       (forM_, join)
import           Data.Function                       (on)
import           Data.List                           (sortBy)
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run                     (safeSpawn)

-- DBUS (for polybar)
import qualified Codec.Binary.UTF8.String            as UTF8
import qualified DBus                                as D
import qualified DBus.Client                         as D

-- sizes
myGapWidth = 5
myTopbarHeight = 10
myBorderWidth = 7

-- fonts
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

-- keys
myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key
myAltMask :: KeyMask
myAltMask = mod1Mask       -- Setting this for use in xprompts

-- apps
myTerminal = "alacritty"   -- Sets default terminal
myTerminalScratchpad = myTerminal ++ " --class scratchpad"
myBrowser = "firefox"
myBrowserClassName = "firefox"
myNetworkManagerClassName = "Nm-connection-editor"
myBluetoothClassName = "Blueman-manager"
myFileManagerClassName = "Nemo"

-- colors
myNormalBorderColor   = "#d1d3d2" -- Border color of normal windows
myFocusedBorderColor  = "#4e937a"  -- Border color of focused windows
myUrgentBorderColor = "#c97b84"
myActiveTextColor = "#268bd2"
myInactiveTextColor = "#073642"
myUrgentTextColor = "#cb4b16"

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" myTerminalScratchpad findTerm manageFloat
                ]
  where
    findTerm   = resource =? "scratchpad"
    manageFloat = customFloating $ W.RationalRect t l w h
               where
                 w = (1/2)
                 h = (1/2)
                 l = (1-w) / 2
                 t = 1/5

outerGaps    = 10
myGaps       = gaps [(U, gap), (R, gap), (L, gap), (D, gap)]
          where gap = fromIntegral outerGaps

mySpacing   = spacingRaw False (Border sp sp sp sp) True (Border sp sp sp sp) True
          where sp = fromIntegral outerGaps

myTabTheme = def
    { fontName              = myFont
    , activeBorderColor     = myFocusedBorderColor
    , activeColor           = myFocusedBorderColor
    , activeTextColor       = myActiveTextColor
    , inactiveBorderColor   = myNormalBorderColor
    , inactiveColor         = myNormalBorderColor
    , inactiveTextColor     = myInactiveTextColor
    }

tall     = renamed [Replace "tall"]
           $ mySpacing
           $ ResizableTall 1 (3/100) (5/9) []

tabs     = renamed [Replace "tabs"]
           $ myGaps
	   $ tabbed shrinkText myTabTheme



-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = smartBorders $ mouseResize $ windowArrange
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)$ myDefaultLayout
             where
               myDefaultLayout = tall
                                 ||| tabs

-- This has to match the configuration in polybar workspace module
myWorkspaces = ["term", "www", "code", "file", "vbox"]
--myWorkspaces = ["1", "2", "3", "4", "5"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

-- dmenu clickable action
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title =? "Mozilla Firefox" --> doShift ( myWorkspaces !! 1 )
     , isDialog --> doCenterFloat
     , (className =? myFileManagerClassName) --> doCenterFloat
     , (className =? myNetworkManagerClassName) --> doCenterFloat
     , (className =? myBluetoothClassName) --> doCenterFloat
     , (className =? "Gucharmap") --> doCenterFloat
     , (className =? myBrowserClassName <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , title =? "Oracle VM VirtualBox Manager" --> doCenterFloat
     , className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 3 )
     , isFullscreen --> doFullFloat
     ] <+> namedScratchpadManageHook myScratchpads

myKeys = [
    -- launch scratchpad terminal. M-<Return> defaults to "Swap the focused window and the master window".
        ("M-<Return>", namedScratchpadAction myScratchpads "terminal")
    -- launch a terminal
        , ("M-S-<Return>", spawn myTerminal)

    -- Kill windows
        , ("M-S-c", kill1)     -- Kill the currently focused client

    -- Quit xmonad.
        , ("M-S-q", io exitSuccess)

    -- Restart xmonad.
        , ("M-q", restart "xmonad" True) -- Recompiles & restart xmonad

    -- Run Prompt
        , ("M1-<Space>", spawn "rofi -no-config -no-lazy-grab -show drun -modi drun -theme ~/.config/polybar/material/scripts/rofi/launcher.rasi") -- Rofi

    -- Floating windows
        , ("M-t", withFocused $ windows . W.sink)

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

   -- Shrink the master area.
        , ("M-h", sendMessage Shrink)

   -- Expand the master area.
        , ("M-l", sendMessage Expand)

   -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-S-m", promote)              -- Swap master
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-f", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-<Space>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Multimedia Keys
        , ("<Print>", spawn "scrotd 0")
        ]

-- Write to FIFO for polybar to read
eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = myWorkspaces
  let wsStr = join $ map (fmt currWs) wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "

-- full screen support.
setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
      supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
      changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)

main :: IO ()
main = do
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
        safeSpawn "mkfifo" ["/tmp/" ++ file]

    xmonad $ ewmh $ def
        { manageHook         = myManageHook <+> manageDocks <+> manageHook desktopConfig
        , modMask            = myModMask
        , terminal           = myTerminal
        , layoutHook         = desktopLayoutModifiers $ showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
	, startupHook        = startupHook desktopConfig <+> setFullscreenSupported
        , handleEventHook    = handleEventHook desktopConfig <+> fullscreenEventHook
        , logHook            = workspaceHistoryHook <+> eventLogHook
        } `additionalKeysP` myKeys
