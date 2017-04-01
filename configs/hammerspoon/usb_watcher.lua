home = os.getenv("HOME")
function usbEventHandler(update)
   local task = nil
   if update.productName == "USB Keyboard" or update.productName == "Das Keyboard" then
      if update.eventType == "added" then
         task = hs.task.new(home .. "/bin/karabiner-switcher",
                            function () end, -- Fake callback
                            function () end, -- Fake stream callback
                            {"0"} -- USB Keyboard profile
         )
         task:start()
         hs.alert("Activated USB Keyboard profile", {radius = 5}, hs.screen.mainScreen(), 10)
      elseif update.eventType == "removed" then
         task = hs.task.new(home .. "/bin/karabiner-switcher",
                            function () end, -- Fake callback
                            function () end, -- Fake stream callback
                            {"1"} -- Internal Keyboard profile
         )
         task:start()
         hs.alert("Activated Internal Keyboard profile", {radius = 5}, hs.screen.mainScreen(), 10)
      end
   end
end
usbWatcher = hs.usb.watcher.new(usbEventHandler)
usbWatcher:start()
