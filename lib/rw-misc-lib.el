;;------------------------------------------------------------------------------
;; misc functions
;;------------------------------------------------------------------------------
(defun rw-test-new-config ()
  "Async open a new emacs, with current file opened"
  (interactive)
  (let ((file (buffer-file-name)))
    (async-shell-command (concat "emacs " file " --debug"))))

(provide 'rw-misc-lib)
