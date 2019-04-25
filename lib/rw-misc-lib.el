;;------------------------------------------------------------------------------
;; misc functions
;;------------------------------------------------------------------------------
(defun rw-test-new-config ()
  "Async open a new emacs, with current file opened."
  (interactive)
  ;; (when (f-exists? rice-wine-configure-file)
  ;;   (org-babel-tangle-file rice-wine-configure-file))
  ;; start emacs
  (let ((file (buffer-file-name)))
    (async-shell-command (concat "emacs " file " --debug"))))

(provide 'rw-misc-lib)
