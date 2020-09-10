;;; init-platform --- Platform-specific settings
;;; Commentary:

;;; Code:
(require 'init-fonts)

;; This must run after window setup or it seems to have no effect.
(add-hook 'window-setup-hook
          (lambda ()
            (when (memq window-system '(mac ns))
              (use-package exec-path-from-shell
                :ensure t
                :config
                (exec-path-from-shell-initialize)
                (exec-path-from-shell-copy-env "GOPATH"))
              ;;(add-to-list 'default-frame-alist '(font . "Input"))
              (set-face-attribute 'default nil :font "Input" :weight 'light)
              (sanityinc/set-frame-font-size 14)
              (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

            (when (memq window-system '(w32))
              ;;(add-to-list 'default-frame-alist '(font . "Hack"))
              (set-face-attribute 'default nil :font "Hack")
              (setq epg-gpg-home-directory "c:/Users/Aaron/AppData/Roaming/GnuPG")
              (setq epg-gpg-program "c:/Users/Aaron/Programs/GnuPG/bin/gpg.exe")
              (setq epg-gpgconf-program "c:/Users/Aaron/Programs/GnuPG/bin/gpgconf.exe")
              (sanityinc/set-frame-font-size 20))

            (when (memq window-system '(x))
              ;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono"))
              (set-face-attribute 'default nil :font "JetBrains Mono")
              (sanityinc/set-frame-font-size 16))

            (when (fboundp 'powerline-reset)
              (powerline-reset))))

(if (eq window-system 'w32)
    (setq ispell-program-name "~/hunspell/bin/hunspell.exe"))

;; Display emoji on Macs where the font is already there.
(when (memq window-system '(mac))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'init-platform)
;;; init-platform.el ends here
