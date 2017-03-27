log = hs.logger.new("hs")
hs.logger.setGlobalLogLevel("warning")
hs.window.animationDuration = 0
hs.grid.setGrid("4x3")
hs.grid.setMargins("0x0")
windowStates = {}

function leftBarHalf(win)
   log.d("Half width left")
   win:move({0, 0, 0.5, 1}, win:screen())
end

function leftBarThird(win)
   log.d("One third width left")
   win:move({0, 0, 1/3, 1}, win:screen())
end

function leftBarTwoThirds(win)
   log.d("Two thirds width left")
   win:move({0, 0, 2/3, 1}, win:screen())
end

function rightBarHalf(win)
   log.d("Half width right")
   win:move({0.5, 0, 0.5, 1}, win:screen())
end

function rightBarThird(win)
   log.d("One third width right")
   win:move({2/3, 0, 1/3, 1}, win:screen())
end

function rightBarTwoThirds(win)
   log.d("Two thirds width right")
   win:move({0.33, 0, 2/3, 1}, win:screen())
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

hs.hotkey.bind("cmd", "left", resizeLeft)
hs.hotkey.bind("cmd", "right", resizeRight)
hs.hotkey.bind("cmd", "up", resizeUp)
hs.hotkey.bind("cmd", "down", resizeDown)
hs.hotkey.bind({"cmd", "shift"}, "right", moveRight)
hs.hotkey.bind({"cmd", "shift"}, "left", moveLeft)
hs.hotkey.bind({"cmd", "shift"}, "up", fullScreen)
hs.hotkey.bind({"ctrl", "shift"}, "f", function() hs.grid.show() end)
