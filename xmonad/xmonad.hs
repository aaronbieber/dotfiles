import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/airborne/.xmobarrc"
xmonad $ defaultConfig { 
	  manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
	, borderWidth = 2
	, focusedBorderColor = "#ffffff"
	, normalBorderColor = "#444444"
	, focusFollowsMouse = False
    }
	`additionalKeysP`
	[ ("C-=", spawn "backlight-adjust +")
	, ("C--", spawn "backlight-adjust -")
	, ("M-p", spawn "dmenu_run -i -nb '#282b57' -nf '#eeeeff' -sb '#555a9e' -fn 'Inconsolata-10'")
	]
