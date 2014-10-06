import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.CycleWS
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig {
          manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , borderWidth = 2
        , focusedBorderColor = "#ffffff"
        , normalBorderColor = "#444444"
        , focusFollowsMouse = False
        , terminal = "gnome-terminal"
        }
        `additionalKeysP` (
        [ ("C-=", spawn "backlight-adjust +")
        , ("C--", spawn "backlight-adjust -")
        , ("M-p", spawn "dmenu_run -i -nb '#282b57' -nf '#eeeeff' -sb '#555a9e' -fn 'Inconsolata-10'")
        , ("S-M-g", spawn "xdg-open `xclip -o`")
        ]
        -- ++ [
        --      (otherModMasks ++ "M-" ++ [key], action tag)
        --    | (tag, key) <- zip (XMonad.workspaces defaultConfig) "123456789"
        --    , (otherModMasks, action) < - [ ("", windows . W.view)
        --                                  , ("S-", windows . W.shift) ]
        --]
        )
