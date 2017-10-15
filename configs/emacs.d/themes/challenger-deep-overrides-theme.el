;;; challenger-deep-overrides-theme.el --- Theme overrides
;;; Commentary:

;;; Code:
(deftheme challenger-deep-overrides
  "Created 2017-04-27.")

(custom-theme-set-variables
 'challenger-deep-overrides
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(org-hide-emphasis-markers t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(safe-local-variable-values (quote ((css-indent-offset . 2) (no-byte-compile t))))
 '(package-selected-packages (quote (challenger-deep-theme helm-make all-the-icons-dired all-the-icons rjsx-mode json-mode lua-mode helm-spotify-plus go-projectile jinja2-mode cyberpunk-theme go-mode org-evil jujube-theme smart-mode-line-powerline-theme smart-mode-line visual-fill-column websocket flycheck powerline evil key-chord color-theme-modern esup counsel-projectile restclient ox-reveal org-tree-slide epresent color-moccur xterm-color nlinum-relative company-shell pandoc-mode virtualenvwrapper counsel helm-swoop groovy-mode octopress zenburn-theme yaml-mode which-key wgrep-ag web-mode w3m use-package twittering-mode sunshine sublime-themes rainbow-mode powerline-evil org-bullets mmm-mode markdown-mode magit highlight-symbol helm-projectile gtags fullframe flycheck-package exec-path-from-shell evil-surround evil-leader evil-jumper evil-indent-textobject emmet-mode elpy dictionary color-theme-sanityinc-tomorrow bpr auto-complete ag))))

(custom-theme-set-faces
 'challenger-deep-overrides
 '(company-tooltip-selection ((t (:foreground "black"))))
 '(company-tooltip-common-selection ((t (:foreground "black" :weight 'bold))))
 '(lazy-highlight ((t (:foreground "gray20"))))
 '(all-the-icons-dired-dir-face ((t (:foreground "khaki2"))))
 '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "grey50" :weight normal))))
 '(sml/git ((t (:background "grey22" :foreground "chartreuse3"))))
 '(default ((t (:family "Hack" :foundry "nil" :slant normal :weight normal :height 140 :width normal))))
 '(org-level-1 ((t (:foreground "#91ddff" :weight bold :height 1.1))))
 '(org-level-1 ((t (:box nil :background nil))))
 '(org-level-2 ((t (:box nil :background nil))))
 '(org-level-3 ((t (:box nil :background nil))))
 '(org-level-4 ((t (:box nil :background nil))))
 '(org-level-5 ((t (:box nil :background nil))))
 '(org-level-6 ((t (:box nil :background nil))))
 '(org-level-7 ((t (:box nil :background nil))))
 '(org-level-8 ((t (:box nil :background nil))))
 '(helm-selection ((t (:background "#65b2ff" :foreground "black")))))

(provide-theme 'challenger-deep-overrides)
;;; challenger-deep-overrides-theme.el ends here
