;;; For all of your web browsing needs.
(when (maybe-require-package 'w3m)
      (setq browse-url-browser-function 'w3m-goto-url-new-session)
      (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."))

(provide 'init-w3m)
