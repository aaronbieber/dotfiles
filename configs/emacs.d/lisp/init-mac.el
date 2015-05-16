;;; Provide settings that only work on Macs.

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Menlo"))
  (set-face-attribute 'default t :font "Menlo")
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

(provide 'init-mac)
