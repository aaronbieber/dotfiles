(when (maybe-require-package 'evil-leader)
  ;; Evil leader must be loaded before evil (as documented).
  (global-evil-leader-mode)

  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    ","  'avy-goto-char-2
    "."  'switch-to-previous-buffer
    ":"  'eval-expression
    "aa" 'align-regexp
    "a=" 'my-align-single-equals
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-toggle
    "c"  'comment-dwim
    "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'narrow-and-set-normal ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'delete-other-windows  ;; C-w o
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

(when (maybe-require-package 'evil-jumper)
  (global-evil-jumper-mode))
(maybe-require-package 'evil-surround)
(maybe-require-package 'evil-indent-textobject)

(when (maybe-require-package 'evil)
  ;; Always use evil mode.
  (evil-mode 1)

  ;; My personal evil settings.
  (setq evil-want-C-u-scroll t)
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-symbol-word-search t)

  ;; Use Emacs mode
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'sunshine-mode 'emacs)
  (evil-set-initial-state 'octopress-mode 'emacs)
  (evil-set-initial-state 'octopress-server-mode 'emacs)
  (evil-set-initial-state 'octopress-process-mode 'emacs)
  (evil-set-initial-state 'ag-mode 'emacs)

  ;; Use insert mode
  (evil-set-initial-state 'twittering-edit-mode 'insert)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)

  (add-to-list 'evil-buffer-regexps '("\\*magit:"))
  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)

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

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)
  (define-key evil-normal-state-map (kbd "C-p")   'helm-projectile)
  (define-key evil-normal-state-map (kbd "-")     'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-]")   'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")    'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")    'show-first-occurrence)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line) ;; I know...

  (evil-define-key 'normal php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'normal php-mode-map (kbd "[n") 'previous-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "[n") 'previous-conflict-marker)

  (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)
  (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
  (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

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
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  (evil-ex-define-cmd "om" 'octopress-status))

(provide 'init-evil)
