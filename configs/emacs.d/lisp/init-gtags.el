;;; init-gtags.el -- Configuration for GNU Global
;;; Commentary:
;;; Code:
(when (and (maybe-require-package 'gtags)
           (maybe-require-package 'bpr))
  ;; Bind some useful keys in the gtags select buffer that evil overrides.
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (evil-define-key 'normal gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
              (evil-define-key 'normal gtags-select-mode-map (kbd "q") 'kill-buffer-and-window)))

  (defun gtags-reindex ()
    "Kick off gtags reindexing."
    (interactive)
    (let* ((root-path (expand-file-name (vc-git-root (buffer-file-name))))
           (gtags-path (concat root-path "GTAGS")))
      (gtags-reindex-process root-path)))

  (defun gtags-reindex-process (path)
    "Internal function triggered by gtags-reindex that recursively generates GTAGS."
    (let ((cmd (concat "cd " (shell-quote-argument path) " && "
                       "find . -type f -iname '*php' | "
                       "gtags -v -f -")))
    (bpr-spawn cmd))))

(provide 'init-gtags)
;;; init-gtags.el ends here
