(when (maybe-require-package 'gtags)
  ;; Bind some useful keys in the gtags select buffer that evil overrides.
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (evil-define-key 'normal gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
              (evil-define-key 'normal gtags-select-mode-map (kbd "q") 'kill-buffer-and-window)))

  (defun gtags-reindex ()
    "Kick off gtags reindexing."
    (interactive)
    (let* ((root-path (project-root))
           (gtags-path (concat root-path "GTAGS")))
      (gtags-reindex-process root-path)))

  (defun gtags-reindex-process (path)
    "Internal function triggered by gtags-reindex that recursively generates GTAGS."
    (let ((gtags-buffer (get-buffer-create "*gtags: reindex*")))
      (with-current-buffer gtags-buffer
        (kill-all-local-variables)
        (setq buffer-read-only t)
        (let ((inhibit-read-only t) (erase-buffer)))
        (display-buffer (current-buffer))
        ;; @TODO make this not PHP specific.
        (let ((cmd (concat "cd " (shell-quote-argument path) " && "
                           "find . -type f -iname '*php' | "
                           "gtags -v -f - &"))
              (map (make-sparse-keymap)))
          ;;(set-keymap-parent map (current-local-map))
          (define-key map "q" #'(lambda () (interactive) (kill-buffer (current-buffer))))
          (use-local-map map)
          (shell-command cmd (current-buffer))
          (if (fboundp 'evil-normal-state)
              (evil-normal-state)))))))

(provide 'init-gtags)
