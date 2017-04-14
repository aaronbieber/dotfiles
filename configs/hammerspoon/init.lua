log = hs.logger.new("hs")
hs.logger.setGlobalLogLevel("warning")
hs.window.animationDuration = 0
hs.grid.setGrid("6x4")
hs.grid.setMargins("0x0")
windowStates = {}

require "usb_watcher"

function setCustomGrid(win, grid, geometry)
   local oldGrid = hs.grid.getGrid()
   hs.grid.setGrid(grid)
   hs.grid.set(win, geometry)
   hs.grid.setGrid(oldGrid)
end

function leftBarHalf(win)
   log.d("Half width left")
   setCustomGrid(win, "2x1", {0, 0, 1, 1})
end

function leftBarThird(win)
   log.d("One third width left")
   setCustomGrid(win, "3x1", {0, 0, 1, 1})
end

function leftBarTwoThirds(win)
   log.d("Two thirds width left")
   setCustomGrid(win, "3x1", {0, 0, 2, 1})
end

function rightBarHalf(win)
   log.d("Half width right")
   setCustomGrid(win, "2x1", {1, 0, 1, 1})
end

function rightBarThird(win)
   log.d("One third width right")
   setCustomGrid(win, "3x1", {2, 0, 1, 1})
end

function rightBarTwoThirds(win)
   log.d("Two thirds width right")
   setCustomGrid(win, "3x1", {1, 0, 2, 1})
end

function topHalf(win)
   local winGeo = win:frame()
   local screenFrame = win:screen():frame()
   win:move({winGeo.x, screenFrame.y, winGeo.w, screenFrame.h / 2})
end

function topThird(win)
   local winGeo = win:frame()
   local screenFrame = win:screen():frame()
   win:move({winGeo.x, screenFrame.y, winGeo.w, screenFrame.h / 3})
end

function bottomHalf(win)
   local winGeo = win:frame()
   local screenFrame = win:screen():frame()
   win:move({winGeo.x, screenFrame.y + (screenFrame.h / 2), winGeo.w, screenFrame.h / 2})
end

function bottomThird(win)
   local winGeo = win:frame()
   local screenFrame = win:screen():frame()
   win:move({winGeo.x, screenFrame.y + ((screenFrame.h / 3) * 2), winGeo.w, screenFrame.h / 3})
end

function indexOf(list, value)
   for index, list_value in pairs(list) do
      if list_value == value then
         return index
      end
   end

   return false
end

function fullScreen()
   local win = hs.window.focusedWindow()
   win:move({0, 0, 1, 1}, win:screen())
   windowStates[win:id()] = nil
end

function cycleState(states)
   local win = hs.window.focusedWindow()
   local id = win:id()
   local state = windowStates[id]
   log.df("Window ID %s", id)

   if not state or not indexOf(states, state) then
      windowStates[id] = states[1]
      log.df("Set win %s to first state", id)
   else
      local i = indexOf(states, state)
      log.df("Got win %s current state index of %s", id, i)
      if i == #states then
         i = 1
      else
         i = i + 1
      end
      windowStates[id] = states[i]
   end

   windowStates[id](win)
end

function resizeLeft()
   cycleState({leftBarHalf, leftBarThird, leftBarTwoThirds})
end

function resizeRight()
   cycleState({rightBarHalf, rightBarThird, rightBarTwoThirds})
end

function resizeUp()
   cycleState({topHalf, topThird})
end

function resizeDown()
   cycleState({bottomHalf, bottomThird})
end

function moveLeft()
   local win = hs.window.focusedWindow()
   local f = win:frame()
   if f.w < win:screen():frame().w / 2 then
      win:move({0-f.w, 0})
   end
end

function moveRight()
   local win = hs.window.focusedWindow()
   local f = win:frame()
   if f.w < win:screen():frame().w / 2 then
      win:move({f.w, 0})
   end
end

do
   io.input("chromeprofile.osascript")
   local script = io.read("*all")

   function switchChromeProfile()
      hs.osascript.applescript(script)
   end
end

hs.hotkey.bind({"ctrl", "shift"}, "m", switchChromeProfile)

hs.hotkey.bind("cmd", "left", resizeLeft)
hs.hotkey.bind("cmd", "right", resizeRight)
hs.hotkey.bind("cmd", "up", resizeUp)
hs.hotkey.bind("cmd", "down", resizeDown)
hs.hotkey.bind({"cmd", "shift"}, "right", moveRight)
hs.hotkey.bind({"cmd", "shift"}, "left", moveLeft)
hs.hotkey.bind({"cmd", "shift"}, "up", fullScreen)
hs.hotkey.bind({"ctrl", "shift"}, "f", function() hs.grid.show() end)

-- 117 is "forward delete" (the "delete" key on a 104-key keyboard)
hs.hotkey.bind({"ctrl", "cmd"}, "delete", hs.caffeinate.startScreensaver)
hs.hotkey.bind({"ctrl", "cmd"}, 117, hs.caffeinate.startScreensaver)
