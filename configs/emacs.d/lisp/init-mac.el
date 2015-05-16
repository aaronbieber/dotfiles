;;; Provide settings that only work on Macs.

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Menlo"))
  (set-face-attribute 'default t :font "Menlo")
  (define-key global-map (kbd "<S-return>") 'toggle-frame-fullscreen)
  (define-key global-map (kbd "<M-S-return>") 'toggle-frame-fullscreen))

(provide 'init-mac)
