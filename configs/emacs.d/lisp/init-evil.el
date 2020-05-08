;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "#"  'server-edit
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-toggle
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    "nw" 'air-org-narrow-to-prose-dwim
    "o"  'delete-other-windows  ;; C-w o
    "p"  'helm-show-kill-ring
    "s"  'ag-project            ;; Ag search from project's root
    "r"  'chrome-reload
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  custom-mode
                  custom-new-theme-mode
                  dired-mode
                  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  hugo-mode
                  hugo-server-mode
                  hugo-process-mode
                  octopress-mode
                  octopress-server-mode
                  octopress-process-mode
                  org-capture-mode
                  sunshine-mode
                  term-mode
                  deadgrep-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(twittering-edit-mode
                  magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)
  (evil-define-key 'normal global-map (kbd "C--")     'air-dired-buffer-dir-or-home)
  (evil-define-key 'normal global-map (kbd "C-`")     (lambda ()
                                                        (interactive)
                                                        (dired (expand-file-name "~"))))
  (evil-define-key 'normal global-map (kbd "C-]")     'gtags-find-tag-from-here)
  (evil-define-key 'normal global-map (kbd "g/")      'occur-last-search)
  (evil-define-key 'normal global-map (kbd "[i")      'show-first-occurrence)
  (evil-define-key 'normal global-map (kbd "S-SPC")   'air-pop-to-org-agenda-default)
  (evil-define-key 'insert global-map (kbd "C-e")     'end-of-line) ;; I know...
  (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'helm-projectile-switch-project)
  (evil-define-key 'insert global-map (kbd "s-d")     'eval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d")     'eval-defun)
  (evil-define-key 'normal global-map (kbd "C-t")     'air-open-eshell)
  (evil-define-key 'normal global-map (kbd "z d")     'dictionary-lookup-definition)
  (evil-define-key 'normal global-map (kbd "\\ \\")   'tiny-menu)
  (evil-define-key 'normal global-map (kbd "\\ a")    (tiny-menu-run-item "org-agendas"))
  (evil-define-key 'normal global-map (kbd "\\ f")    (tiny-menu-run-item "org-files"))
  (evil-define-key 'normal global-map (kbd "\\ t")    (tiny-menu-run-item "org-things"))
  (evil-define-key 'normal global-map (kbd "\\ c")    (tiny-menu-run-item "org-captures"))
  (evil-define-key 'normal global-map (kbd "\\ h")    (tiny-menu-run-item "org-personal-captures"))
  (evil-define-key 'normal global-map (kbd "\\ l")    (tiny-menu-run-item "org-links"))
  (evil-define-key 'normal global-map (kbd "C-=")     'sanityinc/increase-default-font-height)
  (evil-define-key 'normal global-map (kbd "C--")     'sanityinc/decrease-default-font-height)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  (evil-ex-define-cmd "om" 'octopress-status))

(defun air--apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

  (defun next-conflict-marker ()
    (interactive)
    (evil-next-visual-line)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (evil-previous-visual-line))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; PHP
  (evil-define-key 'normal php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'normal php-mode-map (kbd "[n") 'previous-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "[n") 'previous-conflict-marker)

  ;; Dired
  (evil-define-key 'normal dired-mode-map (kbd "C-e") 'dired-toggle-read-only))

(defmacro define-evil-or-global-key (key def &optional state)
  "Define a key KEY with DEF in an Evil map, or in the global map.

If the Evil map for STATE is defined (or `normal' if STATE is not
provided) the key will be defined in that map.  Failing that, it will
be defined globally.

Note that STATE should be provided as an unquoted symbol.

This macro provides a way to override Evil mappings in the appropriate
Evil map in a manner that is compatible with environments where Evil
is not used."
  (let* ((evil-map-name (if state
                            (concat "evil-" (symbol-name state) "-state-map")
                          "evil-normal-state-map"))
         (map (if (boundp (intern evil-map-name))
                  (intern evil-map-name)
                global-map)))
    `(define-key ,map ,key ,def)))

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (air--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (air--apply-evil-other-package-configs))

(provide 'init-evil)
;;; init-evil.el ends here
