;;; Provide settings for specific platforms.
(require 'init-fonts)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Menlo-14"))
  (set-face-attribute 'default t :font "Menlo-14")
  (sanityinc/set-frame-font-size 14)
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

(when (memq window-system '(x))
  (add-to-list 'default-frame-alist '(font . "Fixedsys Excelsior 3.01"))
  (set-face-attribute 'default t :font "Fixedsys Excelsior 3.01")
  (sanityinc/set-frame-font-size 12))

(provide 'init-platform)
