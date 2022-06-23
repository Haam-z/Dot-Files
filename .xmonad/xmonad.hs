
-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ManageDocks ( avoidStruts, docks,docksEventHook, manageDocks, Direction2D(D, L, R, U), ToggleStruts(..) )
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ServerMode

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import Control.Monad ( join, when )

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps ( Direction2D(D, L, R, U), gaps, setGaps,GapMessage(DecGap, ToggleGaps, IncGap))
import XMonad.Layout.Fullscreen ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )

-- Utilities
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.EZConfig (additionalKeysP)


myLogHook = return ()

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "librewolf "    -- Sets default browser

myBorderWidth :: Dimension
myBorderWidth =  1          -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = "#01060b"

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = "#1C2636"

myEmacs :: String
myEmacs  = "emacsclient -c -a 'emacs'"

myFileManager :: String
myFileManager = "nemo"

myTerminalFileManager :: String
myTerminalFileManager =  myTerminal ++ " -e lf "

myLauncher :: String
myLauncher = "j4-dmenu-desktop --dmenu='dmenu -h 24'"

myPassword :: String
myPassword     =  "passmenu"

myClipBoard :: String
myClipBoard = "clipmenu"


myWorkspaces    = ["I", "II", "III", "IV", "V"]

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100



myKeys :: [(String, X ())]
myKeys =
        [ ("M-t",                       spawn myTerminal )
        , ("M-f g",                     spawn myFileManager)
        , ("M-f t",                     spawn myTerminalFileManager)
        , ("M-b",                       spawn myBrowser)
        , ("M-e",                       spawn myEmacs)
        , ("M-d",                     spawn myLauncher )
        , ("M-S-d",                     spawn myClipBoard)
        , ("<XF86AudioPlay>",           spawn "playerctl play-pause")
        , ("<XF86AudioPrev>",           spawn "playerctl previous")
        , ("<XF86AudioNext>",           spawn "playerctl next")
        , ("<XF86AudioMute>",           spawn "amixer -q -D pulse sset Master toggle")
        , ("<XF86AudioRaiseVolume>",    spawn "amixer -q -D pulse sset Master 1%+")
        , ("<XF86AudioLowerVolume>",    spawn "amixer -q -D pulse sset Master 1%-")
        , ("<XF86MonBrightnessUp>",     spawn "brightnessctl s +10%")
        , ("<XF86MonBrightnessDown>",   spawn "brightnessctl s 10-%")
        , ("M-q",                       kill)
        , ("M-<Space>",                 sendMessage NextLayout)
        , ("M-n",                       withFocused $ windows . W.sink)
        , ("M-<Tab>",                   windows W.focusDown)
        , ("M-S-j",                     windows W.swapDown)
        , ("M-S-k",                     windows W.swapUp)
        , ("M-m",                       windows W.focusMaster)
        , ("M-<Return>",                windows W.swapMaster)
        , ("M-h",                       sendMessage Shrink)
        , ("M-l",                       sendMessage Expand)
        , ("M-,",                       sendMessage (IncMasterN 1))
        , ("M-.",                       sendMessage (IncMasterN (-1)))
        , ("M-S-r",                     spawn "xmonad --restart")
        , ("M-C-r",                     spawn "xmonad --recompile")
        , ("M-M1-r",                    io exitSuccess)
        ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp"            --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , className =? "Yad"             --> doCenterFloat
     , isFullscreen -->  doFullFloat
     ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "dunst"
  spawnOnce "picom --experimental-backends"
  spawnOnce "feh --randomize --bg-fill ~/Pictures/wallpapers/Pc_wallpaper/*"
  spawnOnce "emacs --daemon"
  spawnOnce "polybar main"
  spawnOnce "clipmenud"
  spawnOnce "setxkbmap -model pc104 -layout us,ar -option grp:alt_shift_toggle"


main = xmonad $ fullscreenSupport $ docks $ ewmh defaults
defaults = def {
         manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook >> addEWMHFullscreen
        , layoutHook         = gaps [(L,2), (R,2), (U,31), (D,2)] $ spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ smartBorders $ myLayout
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = myLogHook
        } `additionalKeysP` myKeys
