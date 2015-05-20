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
    (let ((gtags-buffer (get-buffer-create "*Gtags*")))
      (with-current-buffer gtags-buffer
        (display-buffer gtags-buffer)
        (setq gtags-window (get-buffer-window gtags-buffer))
        (with-selected-window gtags-window
          (if (> (window-size) 10)
              (shrink-window (- (window-size) 10))
            (enlarge-window (- 10 (window-size)))))
        (widen)
        (kill-all-local-variables)
        (let ((inhibit-read-only t) (erase-buffer)))
        (let ((cmd (concat "cd " (shell-quote-argument path) " && "
                           "find . -type f -iname '*php' | "
                           "gtags -v -f - &"))
              (map (make-sparse-keymap)))
          (shell-command cmd (current-buffer))
          (set-keymap-parent map (current-local-map))
          (define-key map "q" #'(lambda () (interactive) (kill-buffer (current-buffer))))
          (use-local-map map)
          (if (fboundp 'evil-normal-state)
              (evil-normal-state)))))))

(provide 'init-gtags)
