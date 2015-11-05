;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

;(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Just while I'm working on it.
(add-to-list 'load-path (expand-file-name "octopress" user-emacs-directory))
(require 'octopress)

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
(global-visual-line-mode nil)
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq visible-bell t)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
(require 'project-root)
(require 'init-utils)
(require 'init-platform)
(require 'init-global-functions)
(require 'init-elpa)
(require 'init-org)
(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-twitter)
(require 'init-maps)
(require 'init-w3m)
(require 'init-php)
(require 'init-powerline)
(require 'init-flycheck)

(maybe-require-package 'wgrep)
(maybe-require-package 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(when (maybe-require-package 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(when (string= system-type "gnu/linux")
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "google-chrome"))

(when (maybe-require-package 'avy)
  (setq avy-background t))

(maybe-require-package 'ag)
(maybe-require-package 'auto-complete)
(maybe-require-package 'dictionary)
(maybe-require-package 'emmet-mode)
(maybe-require-package 'which-key)
(maybe-require-package 'helm)
(maybe-require-package 'helm-projectile)
(maybe-require-package 'highlight-symbol)
(maybe-require-package 'magit)
(maybe-require-package 'markdown-mode)
(maybe-require-package 'php-extras)
(maybe-require-package 'projectile)
(maybe-require-package 'sublime-themes)
(maybe-require-package 'sunshine)
(maybe-require-package 'web-mode)
(maybe-require-package 'yasnippet)
(maybe-require-package 'zenburn-theme)
(maybe-require-package 'mmm-mode)
(maybe-require-package 'yaml-mode)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
 '((markdown-cl
    :submode emacs-lisp-mode
    :face mmm-declaration-submode-face
    :front "^```cl[\n\r]+"
    :back "^```$")
   (markdown-php
    :submode php-mode
    :face mmm-declaration-submode-face
    :front "^```php[\n\r]+"
    :back "^```$")))

(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php)

;;; Don't display this nag about reverting buffers.
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

;;; Always use guide-key mode, it is awesome.
(which-key-mode 1)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq-default highlight-symbol-idle-delay 1.5)

;;(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq-default ac-dwim nil)
(setq-default ac-use-menu-map t)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)

(defvar projectile-enable-caching t
  "Tell Projectile to cache project file lists.")
(projectile-global-mode)
;; This appears to be necessary only in Linux?
(require 'helm-projectile)

;;; Use Helm all the time.
(setq helm-buffers-fuzzy-matching t)
(helm-mode 1)

(require 'helm)
(define-key helm-buffer-map (kbd "S-SPC") 'helm-toggle-visible-mark)

;;; Use evil surround mode in all buffers.
(global-evil-surround-mode 1)

;;; Helm mode:
(define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; Python mode:
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;; YAsnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/remote-snippets"))
(yas-reload-all)
;;(define-key yas-minor-mode-map (kbd "<tab>") nil)
;;(define-key yas-minor-mode-map (kbd "TAB") nil)
;;(setq tab-always-indent 'yas-expand)
;;(define-key yas-minor-mode-map (kbd "C-l") 'yas-expand)
(define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)

(setq yas-prompt-functions '(yas-completing-prompt
                             yas-ido-prompt
                             yas-dropdown-prompt))

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
            (yas-minor-mode-on)
            (turn-on-eldoc-mode)
            (highlight-symbol-mode)))

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

;;; Let me move the selection like a normal human in the Grizzl results buffer.
(add-hook 'grizzl-mode-hook (lambda ()
                              (define-key *grizzl-keymap* (kbd "C-j") 'grizzl-set-selection-1)
                              (define-key *grizzl-keymap* (kbd "C-k") 'grizzl-set-selection+1)))

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
(point-min) of the buffer to the end, looking for lines that
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

(defun helm-project-files ()
  (interactive)
  (helm-other-buffer '(helm-c-source-projectile-files-list) "*Project Files*"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(custom-safe-themes t)
 '(helm-autoresize-mode t)
 '(helm-buffer-max-length 40)
 '(js-indent-level 2)
 '(lpr-page-header-switches (quote ("-h" "%s" "-F" "-l 65")))
 '(magit-branch-arguments nil)
 '(magit-push-always-verify nil)
 '(octopress-blog-root "/Users/airborne/Blog")
 '(org-agenda-files (quote ("~/Dropbox/org/")) t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-directory "~/Dropbox/org")
 '(org-enforce-todo-dependencies t)
 '(org-log-redeadline (quote time))
 '(org-log-reschedule (quote time))
 '(package-selected-packages
   (quote (zenburn-theme yasnippet web-mode sunshine sublime-themes markdown-mode magit highlight-symbol helm-projectile helm which-key emmet-mode ag avy exec-path-from-shell flycheck w3m evil-jumper yaml-mode wgrep-ag twittering-mode powerline-evil php-extras mmm-mode gtags fullframe evil-surround evil-leader evil-indent-textobject diminish dictionary auto-complete)))
 '(sunshine-location "Brookline, MA")
 '(sunshine-show-icons t)
 '(twittering-use-native-retweet t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-indent-style 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(web-mode-style-padding 0))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-blue ((t (:background "DodgerBlue2" :foreground "DodgerBlue2"))))
 '(term-color-cyan ((t (:background "CadetBlue2" :foreground "CadetBlue2"))))
 '(term-color-green ((t (:background "LimeGreen" :foreground "LimeGreen")))))

;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          ;; shift
          (if (= x 2)
              (setq tkey "S-"))
          ;; alt
          (if (= x 3)
              (setq tkey "M-"))
          ;; alt + shift
          (if (= x 4)
              (setq tkey "M-S-"))
          ;; ctrl
          (if (= x 5)
              (setq tkey "C-"))
          ;; ctrl + shift
          (if (= x 6)
              (setq tkey "C-S-"))
          ;; ctrl + alt
          (if (= x 7)
              (setq tkey "C-M-"))
          ;; ctrl + alt + shift
          (if (= x 8)
              (setq tkey "C-M-S-"))

          ;; arrows
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
          ;; home
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
          ;; end
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
          ;; page up
          (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
          ;; page down
          (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
          ;; insert
          (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; delete
          (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; f1
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
          ;; f2
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
          ;; f3
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
          ;; f4
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
          ;; f5
          (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
          ;; f6
          (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
          ;; f7
          (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
          ;; f8
          (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
          ;; f9
          (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
          ;; f10
          (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
          ;; f11
          (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
          ;; f12
          (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
          ;; f13
          (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
          ;; f14
          (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
          ;; f15
          (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
          ;; f16
          (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
          ;; f17
          (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
          ;; f18
          (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
          ;; f19
          (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
          ;; f20
          (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

          (setq x (+ x 1))
          ))
      )
  )

(put 'narrow-to-region 'disabled nil)
(load-theme 'spolsky t)
(require 'init-linum)

(when (maybe-require-package 'diminish)
  (require 'diminish)
  (eval-after-load "highlight-symbol"
    '(diminish 'highlight-symbol-mode))
  (diminish 'helm-mode)
  (diminish 'which-key-mode)
  (diminish 'mmm-mode)
  (diminish 'undo-tree-mode))

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(my-powerline-default-theme)

(provide 'emacs)
;;; emacs ends here
