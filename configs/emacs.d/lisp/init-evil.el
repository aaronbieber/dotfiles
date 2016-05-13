;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
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

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  octopress-mode
                  octopress-server-mode
                  octopress-process-mode
                  sunshine-mode))
    (add-to-list 'evil-emacs-state-modes mode))

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
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>")   'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)
  (define-key evil-normal-state-map (kbd "C-p")   'helm-projectile)
  (define-key evil-normal-state-map (kbd "-")     'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-]")   'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")    'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")    'show-first-occurrence)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line) ;; I know...

  (define-key evil-normal-state-map (kbd "S-SPC") 'air-pop-to-org-agenda)

  (require 'air-menu-run)
  (setq air-menu-run-items
        '(("org-things"   ("Things"
                           ((?t "Tag"   org-tags-view)
                            (?i "ID"    air-org-goto-custom-id))))
          ("org-files"    ("Files"
                           ((?t "TODO"  (lambda () (air-pop-to-org-todo nil)))
                            (?n "Notes" (lambda () (air-pop-to-org-notes nil)))
                            (?v "Vault" (lambda () (air-pop-to-org-vault nil))))))
          ("org-captures" ("Captures"
                           ((?t "TODO"  air-org-task-capture)
                            (?n "Note"  (lambda () (interactive) (org-capture nil "n"))))))))
  (evil-define-key 'normal global-map (kbd "\\ \\") 'air-menu-run)
  (evil-define-key 'normal global-map (kbd "\\ f") (lambda () (interactive) (air-menu-run "org-files")))
  (evil-define-key 'normal global-map (kbd "\\ t") (lambda () (interactive) (air-menu-run "org-things")))
  (evil-define-key 'normal global-map (kbd "\\ c") (lambda () (interactive) (air-menu-run "org-captures")))

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

(use-package evil
  :ensure t
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
