;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
       ";;\n"
       ";; __          __  _                            \n"
       ";; \\ \\        / / | |                           \n"
       ";;  \\ \\  /\\  / /__| | ___ ___  _ __ ___   ___   \n"
       ";;   \\ \\/  \\/ / _ \\ |/ __/ _ \\| '_ ` _ \\ / _ \\  \n"
       ";;    \\  /\\  /  __/ | (_| (_) | | | | | |  __/_ \n"
       ";;     \\/  \\/ \\___|_|\\___\\___/|_| |_| |_|\\___(_)\n"))

;; Leave this here, or package.el will just add it again.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes (expand-file-name "lisp" user-emacs-directory) t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(if (not (eq window-system 'w32))
    (add-to-list 'exec-path "/usr/local/bin"))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

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
(setq show-paren-delay 0)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)

;; Allow "confusing" functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defun air--delete-trailing-whitespace-in-prog-and-org-files ()
  "Delete trailing whitespace if the buffer is in `prog-' or `org-mode'."
  (if (or (derived-mode-p 'prog-mode)
          (derived-mode-p 'org-mode))
      (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'air--delete-trailing-whitespace-in-prog-and-org-files)

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
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-platform)
(require 'init-global-functions)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))

;;; Required by init-maps, so it appears up here.
(use-package tiny-menu
  :ensure t
  :commands (tiny-menu-run-item)
  :config
  (setq tiny-menu-items
        '(("reverts"      ("Revert"
                           ((?r "This buffer"     revert-buffer)
                            (?o "All Org buffers" org-revert-all-org-buffers))))
          ("org-things"   ("Org Things"
                           ((?t "Tag"       air-org-display-any-tag)
                            (?i "ID"        air-org-goto-custom-id)
                            (?k "Keyword"   org-search-view)
                            (?h "Headings"  air-org-helm-headings)
                            (?d "Directs"   air-org-display-directs)
                            (?m "Managers"  air-org-display-managers)
                            (?e "Engineers" air-org-display-engineers))))
          ("org-agendas"  ("Org Agenda Views"
                           ((?a "All"    air-pop-to-org-agenda-default)
                            (?r "Review" air-pop-to-org-agenda-review)
                            (?h "Home"   air-pop-to-org-agenda-home)
                            )))
          ("org-links"    ("Org Links"
                           ((?c "Capture"      org-store-link)
                            (?l "Insert first" air-org-insert-first-link)
                            (?L "Insert any"   org-insert-link)
                            (?i "Custom ID"    air-org-insert-custom-id-link))))
          ("org-files"    ("Org Files"
                           ((?t "TODO"  (lambda () (air-pop-to-org-todo nil)))
                            (?n "Notes" (lambda () (interactive) (air-pop-to-org-notes nil)))
                            (?v "Vault" (lambda () (interactive) (air-pop-to-org-vault nil))))))
          ("org-captures" ("Org Captures"
                           ((?c "Task"    (lambda () (interactive) (org-capture nil "c")))
                            (?b "Backlog" (lambda () (interactive) (org-capture nil "b")))))))))

;;; Larger package-specific configurations.
(require 'init-fonts)
(require 'init-evil)
(require 'init-maps)
(require 'init-w3m)
(require 'init-flycheck)
(require 'init-tmux)
;(require 'init-gtags)
;(require 'init-twitter)
;(require 'init-php)

;; My packages (make sure they're cloned into "lisp")
(require 'fence-edit)
(require 'hugo)
(require 'periodic-commit-minor-mode)

;; Utilities
(use-package s
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

(use-package visual-fill-column
  :ensure t
  :defer t)

;; Org Mode
(require 'init-org)

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :defer t)

(use-package helm-make
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-c m") 'helm-make-projectile))

(use-package dired
  :defer t
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

  (defun air-dired-buffer-dir-or-home ()
    "Open dired to the current buffer's dir, or $HOME."
    (interactive)
    (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
                   (expand-file-name "~"))))
      (dired cwd)))

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode t)
                               (all-the-icons-dired-mode t)))
  (define-key dired-mode-map (kbd "RET")     'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")       (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.")     'dired-omit-mode)
  (define-key dired-mode-map (kbd "c")       'find-file)
  (define-key dired-mode-map (kbd "/")       'counsel-grep-or-swiper)
  (define-key dired-mode-map (kbd "?")       'evil-search-backward)
  (define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only))

(eval-after-load 'wdired
  (add-hook 'wdired-mode-hook 'evil-normal-state))

(use-package elpy
  :ensure t
  :mode "\\.py\\'"
  :config
  (elpy-enable))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (if (not (string-match "go" compile-command))
                                (set (make-local-variable 'compile-command)
                                     "go build -v && go test -v && go vet"))
                            (setq compilation-read-command nil)
                            (add-hook 'before-save-hook 'gofmt-before-save nil t)
                            (define-key go-mode-map (kbd "C-c C-C") 'compile))))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (defun rjsx-mode-config ()
    "Configure RJSX Mode"
    (define-key rjsx-mode-map (kbd "C-j") 'rjsx-delete-creates-full-tag))
  (add-hook 'rjsx-mode-hook 'rjsx-mode-config))

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'"
  :config
  (c-set-offset 'label 4))

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (add-hook 'css-mode-hook (lambda ()
                             (rainbow-mode))))

(use-package wgrep
  :ensure t
  :defer t
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
  :commands (ag ag-project)
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
  :defer t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC")          'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
  (define-key helm-read-file-map (kbd "C-k")  'helm-find-files-up-one-level))

(use-package helm-org
  :ensure t
  :commands helm-org-agenda-files-headings)

(use-package company
  :ensure t
  :defer t
  :config
  (defun org-keyword-backend (command &optional arg &rest ignored)
    "Company backend for org keywords.

COMMAND, ARG, IGNORED are the arguments required by the variable
`company-backends', which see."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (let ((p (company-grab-line "^#\\+\\(\\w*\\)" 1)))
                     (if p (cons p t)))))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends 'org-keyword-backend)

  (setq company-idle-delay 0.4)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (global-company-mode))

(use-package counsel
  :ensure t
  :defer t)

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 20))

(use-package dictionary
  :ensure t
  :defer t)

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
              (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point))))

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile helm-projectile-switch-project))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "<C-return>") 'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '")      'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1")      'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2")      'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3")      'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4")      'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5")      'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6")      'markdown-insert-header-atx-6)

  (add-hook 'markdown-mode-hook (lambda ()
                                  (visual-line-mode t)
                                  (set-fill-column 80)
                                  (turn-on-auto-fill)
                                  (yas-minor-mode-on)
                                  (hugo-minor-mode t)
                                  ;; Don't wrap Liquid tags
                                  (setq auto-fill-inhibit-regexp (rx "{" (? "{") (1+ (or "%" "<" " ")) (1+ letter)))
                                  (flyspell-mode))))

(use-package sublime-themes
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :mode "\\(?:\\(?:\\.\\(?:html\\|twig\\)\\)\\)\\'"
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2)

  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-php-extras
                    ac-source-yasnippet
                    ac-source-gtags
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))
          ("css" . (ac-source-css-property
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-style-padding 2)
              (yas-minor-mode t)
              (emmet-mode)
              (flycheck-add-mode 'html-tidy 'web-mode)
              (flycheck-mode)))

  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))))))

(use-package yaml-mode
  :ensure t
  ;; .yaml or .yml
  :mode "\\(?:\\(?:\\.y\\(?:a?ml\\)\\)\\)\\'")

(use-package yasnippet
  :ensure t
  :defer t
  :config
  ;;(yas-reload-all)
  (setq tab-always-indent 'complete)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

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
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

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

(use-package undo-tree
  :ensure t
  :defer t
  :diminish t
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        (list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

(use-package atomic-chrome
  :ensure t
  :defer t
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (add-hook 'atomic-chrome-edit-mode-hook (lambda ()
                                            (turn-off-auto-fill)
                                            (visual-fill-column-mode t))))

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; All programming modes
(defun air--set-up-prog-mode ()
  "Configure global `prog-mode'."
  (setq-local comment-auto-fill-only-comments t)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'air--set-up-prog-mode)

;;; If `display-line-numbers-mode' is available (only in Emacs 26),
;;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

;;; Python mode:
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(defun air-python-setup ()
  "Configure Python environment."
  (let* ((root (air--get-vc-root))
         (venv-name (car (last (remove "" (split-string root "/")))))
         (venv-path (expand-file-name venv-name venv-location)))
    (if (and venv-name
             venv-path
             (file-directory-p venv-path))
        (venv-workon venv-name))))

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
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (air-python-setup)))

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

(defun air--eshell-clear ()
  "Clear an eshell buffer and re-display the prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun air--eshell-mode-hook ()
  "Eshell mode settings."
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-l") 'air--eshell-clear)
  (define-key eshell-mode-map (kbd "C-d") (lambda () (interactive)
                                            (kill-this-buffer)
                                            (if (not (one-window-p))
                                                (delete-window))))
  (set (make-local-variable 'pcomplete-ignore-case) t)
  (set (make-local-variable 'company-backends)
       '((esh-autosuggest))))

(add-hook 'eshell-mode-hook 'air--eshell-mode-hook)

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

;;; Javascript mode:
(add-hook 'javascript-mode-hook (lambda ()
                                  (set-fill-column 120)
                                  (turn-on-auto-fill)
                                  (setq js-indent-level 2)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)
                            (setq indent-tabs-mode nil)))

(server-start)

(and (fboundp 'atomic-chrome-start-server)
  (atomic-chrome-start-server))

(provide 'init)
;;; init.el ends here
