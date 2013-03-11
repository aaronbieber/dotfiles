import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

--	, focusedBorderColor = "#87db00"
--	, normalBorderColor = "#cccccc"

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/airborne/.xmobarrc"
xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
	, borderWidth = 2
	, focusedBorderColor = "#ffffff"
	, normalBorderColor = "#444444"
	, focusFollowsMouse = False
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock") --mod4mask is the windows key
    , ((0, xK_Print), spawn "gnome-screenshot")
    ]

