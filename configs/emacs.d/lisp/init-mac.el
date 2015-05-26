;;; Provide settings that only work on Macs.
(require 'init-fonts)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Menlo"))
  (set-face-attribute 'default t :font "Menlo")
  (sanityinc/set-frame-font-size 14)
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

(provide 'init-mac)
