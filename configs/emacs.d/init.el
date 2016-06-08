;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")
(require 'init-utils)
(require 'init-elpa)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)

;; The OS X visible bell is buggy as hell.
(defvar air-bell-ringing nil
  "Whether my visual bell is currently being rung.

This prevents simultaneously ringing two bells and falling into a race
condition where the bell visualization never clears.")

(setq ring-bell-function (lambda ()
                           (if (not air-bell-ringing)
                               (let* ((bg (face-background 'default))
                                      (fg (face-foreground 'default))
                                      (reset `(lambda ()
                                                (set-face-background 'default ,bg)
                                                (set-face-foreground 'default ,fg)
                                                (setq air-bell-ringing nil))))

                                 (set-face-background 'default "NavajoWhite4")
                                 ;(set-face-foreground 'default "black")
                                 (setq air-bell-ringing t)

                                 (run-with-timer 0.05 nil reset)))))

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-platform)
(require 'init-global-functions)

(require 'diminish)
(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-twitter)
(require 'init-maps)
(require 'init-w3m)
(require 'init-php)
(require 'init-powerline)
(require 'init-flycheck)
(require 'init-tmux)

(require 'markdown-preview-mode)
(add-hook 'markdown-preview-mode-hook
          (lambda ()
            (setq markdown-preview-template
                  (expand-file-name "~/.emacs.d/markdown-preview.html" user-emacs-directory))
            (setq markdown-preview-style
                  "http://aaronbieber.com/assets/styles/github-markdown.css")))

(add-to-list 'load-path (expand-file-name "fence-edit" user-emacs-directory))
(require 'fence-edit)

;; Utilities
(use-package s :ensure t)
(use-package dash :ensure t)

;; Org prerequisites
(use-package visual-fill-column
  :ensure t)

(require 'init-org)

;; Just while I'm working on it.
;;(add-to-list 'load-path (expand-file-name "octopress" user-emacs-directory))
(use-package octopress
  :ensure t
  :config
  (setq octopress-blog-root (expand-file-name "~/Blog")))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package elpy
  :ensure t)

(use-package groovy-mode
  :ensure t
  :config
  (c-set-offset 'label 4))

(use-package rainbow-mode
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
        (evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
        (evil-motion-state)))
  (ad-activate 'wgrep-finish-edit))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :defer t
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  ;(setq company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "yellow2" :foreground "#c82829"))))
  ;(setq company-tooltip-selection ((t (:background "yellow2"))))
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper)
  :config
  (setq ivy-height 20))

(use-package dictionary :ensure t)
(use-package emmet-mode :ensure t)
(use-package flycheck :ensure t)
(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '") 'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6))

(use-package php-extras :ensure t :defer t)
(use-package sublime-themes :ensure t)
(use-package sunshine
  :ensure t
  :commands sunshine-forecast
  :config
  (defun get-string-from-file (file-path)
    "Return FILE-PATH's contents."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
  (setq sunshine-appid (get-string-from-file
                        (expand-file-name "sunshine-appid" user-emacs-directory)))
  (setq sunshine-location "Brookline, MA")
  (setq sunshine-show-icons t))

(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (add-hook 'twittering-mode-hook
            (lambda ()
              (define-key twittering-mode-map (kbd ",o") 'delete-other-windows)
              (define-key twittering-mode-map (kbd ",b") 'helm-mini)
              (define-key twittering-mode-map (kbd "C-c r") 'twittering-retweet)))
  (setq twittering-use-native-retweet t)
  (setq twittering-default-show-replied-tweets 3)
  (setq twittering-use-icon-storage t))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

(use-package zenburn-theme :ensure t :defer t)
(use-package mmm-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package fzf
  :ensure t
  :config
  (require 'projectile)
  (defun air-fzf-projectile-project-root ()
    "Start `fzf' from the root of a known Projectile project."
    (interactive)
    (fzf-directory (completing-read "Select a project: "
                                    (if (projectile-project-p)
                                        (cons (abbreviate-file-name (projectile-project-root))
                                              (projectile-relevant-known-projects))
                                      projectile-known-projects))))

  (require 'init-evil) ;; For safety
  (define-evil-or-global-key (kbd "C-p") 'fzf)
  (define-evil-or-global-key (kbd "C-S-p") 'air-fzf-projectile-project-root))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package mmm-mode
  :ensure t
  :defer t
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((markdown-cl
      :submode emacs-lisp-mode
      :face mmm-declaration-submode-face
      :front "^~~~cl[\n\r]+"
      :back "^~~~$")
     (markdown-php
      :submode php-mode
      :face mmm-declaration-submode-face
      :front "^```php[\n\r]+"
      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php))

(use-package sublime-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

;;; Python mode:
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location
        (expand-file-name "~/Projects/virtualenvs/")))

(add-hook 'python-mode-hook
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
            (defun python-shell-accept-process-output (process &optional timeout regexp)
              "Redefined to actually work."
              (let ((regexp (or regexp comint-prompt-regexp)))
                (catch 'found
                  (while t
                    (when (not (accept-process-output process timeout))
                      (throw 'found nil))
                    (when (= (point) (cdr (python-util-comint-last-prompt)))
                      (throw 'found t))))))

            ;; Additional settings follow.
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;; The Emacs Shell
(defun company-eshell-history (command &optional arg &rest ignored)
  "Complete from shell history when starting a new line.

Provide COMMAND and ARG in keeping with the Company Mode backend spec.
The IGNORED argument is... Ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(add-hook 'eshell-mode-hook
          (lambda ()
            (set (make-local-variable 'pcomplete-ignore-case) t)
            (set (make-local-variable 'company-backends)
                 '((company-shell company-eshell-history)))))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Emmet mode:
(add-hook 'emmet-mode-hook
          (lambda ()
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (yas-minor-mode t)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Twittering mode:
(setq twittering-use-master-password t)
(add-hook 'twittering-mode-hook (lambda ()
                                  (define-key twittering-mode-map (kbd "C-c C-a") 'twittering-favorite)
                                  (define-key twittering-mode-map (kbd ",b") 'helm-mini)))

(add-hook 'twittering-edit-mode-hook (lambda ()
                                       (flyspell-mode)))

;;; Javascript mode:
(add-hook 'javascript-mode-hook (lambda ()
                                  (set-fill-column 120)
                                  (turn-on-auto-fill)
                                  (setq js-indent-level 2)))

;;; Markdown mode:
(add-hook 'markdown-mode-hook (lambda ()
                                (set-fill-column 80)
                                (turn-on-auto-fill)
                                (flyspell-mode)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)
                            (setq indent-tabs-mode nil)))

(defun find-php-functions-in-current-buffer ()
  "Find lines that appear to be PHP functions in the buffer.

This function performs a regexp forward search from the top
\(point-min) of the buffer to the end, looking for lines that
appear to be PHP function declarations.

The return value of this function is a list of cons in which
the car of each cons is the bare function name and the cdr
is the buffer location at which the function was found."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (save-match-data
        (while (re-search-forward  "^ *\\(public \\|private \\|protected \\|static \\)*?function \\([^{]+\\)" nil t)
          (let* ((fn-name (save-match-data (match-string-no-properties 2)))
                 (fn-location (save-match-data (match-beginning 0))))
            (setq res
                  (append res
                          (list `(,fn-name . ,fn-location)))))))
      res)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide-emphasis-markers t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (fzf xterm-color nlinum-relative company-shell pandoc-mode virtualenvwrapper counsel helm-swoop groovy-mode octopress zenburn-theme yaml-mode which-key wgrep-ag web-mode w3m use-package twittering-mode sunshine sublime-themes rainbow-mode powerline-evil php-extras org-bullets mmm-mode markdown-mode magit highlight-symbol helm-projectile gtags fullframe flycheck-package exec-path-from-shell evil-surround evil-leader evil-jumper evil-indent-textobject emmet-mode elpy dictionary color-theme-sanityinc-tomorrow bpr auto-complete ag)))
 '(safe-local-variable-values (quote ((css-indent-offset . 2) (no-byte-compile t)))))

(put 'narrow-to-region 'disabled nil)
(diminish 'undo-tree-mode)

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(load-theme 'gruvbox)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:foreground "ivory1"))))
 '(term-color-black ((t (:foreground "gray80"))))
 '(term-color-cyan ((t (:foreground "cyan2"))))
 '(term-color-green ((t (:foreground "OliveDrab3"))))
 '(term-color-yellow ((t (:foreground "gold1")))))
