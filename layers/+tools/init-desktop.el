;;; emacs sessions
(use-package desktop
  ;; use seldom
  :config
  (setq desktop-dirname rice-wine-dir)
  (setq desktop-path (list rice-wine-dir))
  
  (message "desktop: %s\n" desktop-dirname)
  
  (defun rw-desktop-save ()
    (interactive)
    (desktop-save-in-desktop-dir))
  
  (defun rw-desktop-read ()
    (interactive)
    (desktop-read desktop-dirname))
  
  (setq desktop-globals-to-save
        (append '((extended-command-history . 128)
                  (file-name-history        . 128)
                  (ido-last-directory-list  . 128)
                  (ido-work-directory-list  . 128)
                  (ido-work-file-list       . 128)
                  (grep-history             . 128)
                  (compile-history          . 128)
                  (minibuffer-history       . 128)
                  (query-replace-history    . 128)
                  (read-expression-history  . 128)
                  (regexp-history           . 128)
                  (regexp-search-ring       . 128)
                  (search-ring              . 128)
                  (comint-input-ring        . 128)
                  (shell-command-history    . 128)
                  (evil-ex                  . 128)
                  desktop-missing-file-warning
                  register-alist)))

  (setq desktop-restore-frames nil) ; not save frame and window configuration.
  (setq desktop-restore-eager 100)
  
  (desktop-save-mode)

  (message "desktop: %s\n" desktop-dirname)
  )

(provide 'init-desktop)
