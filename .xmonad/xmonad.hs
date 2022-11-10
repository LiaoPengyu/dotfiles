{-# LANGUAGE BangPatterns #-}
-- Base
import           Control.DeepSeq                     (deepseq)
import           System.Exit                         (exitSuccess)
import           System.IO                           (hPutStrLn)
import           XMonad
import           XMonad.Core
import           XMonad.Operations                   (restart)
import qualified XMonad.StackSet                     as W

-- Actions
import           XMonad.Actions.CycleWindows (rotUp, rotUnfocusedDown,rotUnfocusedUp, rotUnfocused')
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown, rotSlaves')
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
import           XMonad.Hooks.ManageDocks            (SetStruts (..),
                                                      ToggleStruts (..),
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
import           XMonad.Layout.TwoPanePersistent
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
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR, NBFULL, NOBORDERS, SMARTBORDERS))
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
myIdeClassName = "Emacs"

-- colors
myNormalBorderColor   = "#d1d3d2" -- Border color of normal windows
myFocusedBorderColor  = "#4e937a"  -- Border color of focused windows
myUrgentBorderColor = "#c97b84"
myActiveTextColor = "#fdfdfd"
myInactiveTextColor = "#0a0a0a"
myUrgentTextColor = "#cb4b16"
myXmobarActiveColor = "#d88666"
myXmobarWithWindowColor = "#d88666"

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" myTerminalScratchpad findTerm manageFloat ]
  where
    findTerm   = resource =? "scratchpad"
    manageFloat = customFloating $ W.RationalRect t l w h
               where
                 w = (1)
                 h = (4/9)
                 t = 0
                 l = 1 - h

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

tall     = renamed [Replace "Tall"]
           $ mySpacing
           $ ResizableTall 1 (3/100) (5/9) []

twoPane =  renamed [Replace "TwoPane"] $
           TwoPanePersistent Nothing (3/100) (5/9)

tabs     = renamed [Replace "Tabs"]
           $ myGaps
           $ tabbed shrinkText myTabTheme



-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Iosevka Nerd Font:size=55"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = avoidStruts 
               $ smartBorders
               $ mouseResize 
               $ windowArrange
               $ mkToggle (single NBFULL)
               $ mkToggle (single FULL)
               $ myDefaultLayout
             where
               myDefaultLayout = tall
                                 ||| twoPane
                                 ||| tabs

myWorkspacesIcon = ["\62601 ", "\63288 ", "\64271 ", "\61563 ", "\63159 "]
--myWorkspacesIcon = ["", "", "﬏", "", ""]
myWorkspaces = ["term", "www", "code", "file", "vbox"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ title =? "Mozilla Firefox" --> doShift ( myWorkspaces !! 1 )
     , isDialog --> doCenterFloat
     , className =? myIdeClassName --> doShift ( myWorkspaces !! 2 )
     , className =? myFileManagerClassName --> doCenterFloat
     , className =? myNetworkManagerClassName --> doCenterFloat
     , className =? myBluetoothClassName --> doCenterFloat
     , className =? "Gucharmap" --> doCenterFloat
     , title =? "htop" --> doCenterFloat
     , title =? "ncmpcpp" --> doCenterFloat
     , (className =? myBrowserClassName <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , title =? "Oracle VM VirtualBox Manager" --> doCenterFloat
     , className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 3 )
     , isFullscreen --> doFullFloat
     ] <+> namedScratchpadManageHook myScratchpads

-- Get current layout of focused WS
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
    workspaces <- gets windowset
    return $ description . W.layout . W.workspace . W.current $ workspaces

-- Force secondary for TwoPane. Unless the secondary get focused, the window wouldn't swap. 
forceSecondaryRefresh = do
    workspaces <- gets windowset
    let isMaster = null . W.up . fromJust $ W.stack . W.workspace . W.current $ workspaces in
      case isMaster of
          True -> windows swapTwoPane <+> windows swapTwoPane
          False -> return () 
        

-- Swap between current and master for TwoPane
swapTwoPane :: W.StackSet i l a s sd -> W.StackSet i l a s sd
swapTwoPane = W.modify' $ \c -> case c of
    W.Stack t [] rs -> W.Stack x xs [] where (x:xs) = reverse (t:rs)
    W.Stack t ls rs -> W.Stack x [] (rs ++ xs) where (x:xs) = reverse (t:ls)
    


-- Still lazy .... How to make unfocused secondary work?
rotUnfocused :: X ()
rotUnfocused = windows . W.modify' $ rotUnfocusedHelper rotUp

-- The unfocused rotation on a stack.
rotUnfocusedHelper :: ([a] -> [a]) -> W.Stack a -> W.Stack a
rotUnfocusedHelper _ s@(W.Stack _ [] []) = s
rotUnfocusedHelper f s@(W.Stack _ [] !rs) = s'
    where s'@(W.Stack _ _ !rs') = rotSlaves' f s                 -- Master has focus
rotUnfocusedHelper f s@(W.Stack t ls rs) = rotUnfocused' f s

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
        , ("M1-<Space>", spawn "rofi -no-config -no-lazy-grab -show drun -modi drun -theme ~/.config/rofi/quick-launcher.rasi") -- Rofi

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
        , ("M-r", rotUnfocused)
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-S-m", promote)              -- Swap master
        , ("M-j", do 
        layout <- getActiveLayoutDescription
        case layout of
            "TwoPane" -> windows swapTwoPane 
            _ -> windows W.focusDown)     -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Tab>", do 
        layout <- getActiveLayoutDescription
        case layout of
            "TwoPane" -> rotSlavesDown <+> forceSecondaryRefresh 
            _ -> windows W.focusDown)      -- Rotate all the seconardary windows
        , ("M-S-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-f", sendMessage (MT.Toggle FULL)) -- Toggles full with status bar
        , ("M-<Space>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Multimedia Keys
        , ("<Print>", spawn "scrotd 0")
        ]

-- Write to FIFO for polybar to read
myPolybarEventLogHook = do
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

myStartupHook = setWMName "LG3D"

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

-- Get the (index, icon) of the WS
convertToIcon ws = (icon, index)
    where
      index = fromJust $ M.lookup ws myWorkspaceIndices
      icon = myWorkspacesIcon !! (index-1)

-- Xmobar clickable action
clickableWs "" = ""
clickableWs ws = "<action=xdotool key super+"++show i++">"++icon++"</action>"
    where
      (icon, i) = convertToIcon ws
-- Click for next layout
clickableLayout l = "<action=xdotool key super+space>"++l++"</action>"


myCurrentLogFormatter w = xmobarColor myXmobarActiveColor "" $ wrap "[" "]" icon
    where (icon, _) = convertToIcon w
myVisibleLogFormatter "NSP" = ""
myVisibleLogFormatter w     = wrap " " " " $ clickableWs w
myHiddenNWLogFormatter "NSP" = ""
myHiddenNWLogFormatter w     = wrap " " " " $ clickableWs w
myHiddenLogFormatter "NSP" = ""
myHiddenLogFormatter w = xmobarColor myXmobarWithWindowColor "" $ wrap " " " " $ clickableWs w
myLayoutLogFormatter = clickableLayout

main :: IO ()
main = do
    -- Create FIFO file for Polybar
    -- forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    --     safeSpawn "mkfifo" ["/tmp/" ++ file]

    xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
    xmonad $ ewmhFullscreen . ewmh $ def
        { manageHook         = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook <+> setFullscreenSupported
        , handleEventHook    = handleEventHook desktopConfig 
        , logHook            =
                             --workspaceHistoryHook <+>
	                         --myPolybarEventLogHook <+>
                   -- (myDebugHook xmproc) <+>
			       dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = myCurrentLogFormatter          -- Current workspace in xmobar
                        , ppVisible = myVisibleLogFormatter          -- Visible but not current workspace
                        , ppHidden = myHiddenLogFormatter            -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = myHiddenNWLogFormatter -- Hidden workspaces (no windows)
                        , ppLayout = myLayoutLogFormatter            -- Layout
                        , ppSep =  " | "
                        , ppTitle = shorten 60                       -- Title of active window in xmobar
                        , ppUrgent = xmobarColor myUrgentTextColor "" . wrap "<" ">" -- Urgent workspace
                        }
        } `additionalKeysP` myKeys
