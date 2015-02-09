(when (maybe-require-package 'evil-leader)
  ;; Evil leader must be loaded before evil (as documented).
  (global-evil-leader-mode)

  ;; Configure evil-leader
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    "."  'switch-to-previous-buffer
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "b"  'helm-mini
    "B"  'magit-blame-mode
    "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-semantic-or-imenu
    "g"  'magit-status
    "l"  'whitespace-mode
    "nn" 'narrow-and-set-normal ;; "narrow normal"
    "nw" 'widen                 ;; "narrow widen"
    "o"  'delete-other-windows
    "p"  'put-from-x-clipboard
    "s"  'ag-project            ;; "search"
    "r"  'font-lock-fontify-buffer
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))

(maybe-require-package 'evil-jumper)
(maybe-require-package 'evil-surround)
(maybe-require-package 'evil-indent-textobject)

(when (maybe-require-package 'evil)
  ;; Always use evil mode.
  (evil-mode 1)

  ;; My personal evil settings.
  (add-to-list 'evil-buffer-regexps '("\\*Sunshine\\*"))
  (add-to-list 'evil-buffer-regexps '("\\*magit:"))

  (setq evil-want-C-u-scroll t)
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-symbol-word-search t)

  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'twittering-edit-mode 'insert)

  (evil-add-hjkl-bindings ag-mode-map 'normal
    "n"   'evil-search-next
    "N"   'evil-search-previous
    "RET" 'compile-goto-error)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
    "RET" 'org-agenda-switch-to)

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
  (define-key evil-normal-state-map (kbd "-")   'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-]") 'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")  'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")  'show-first-occurrence)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

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
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(provide 'init-evil)
