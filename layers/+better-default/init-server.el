;;------------------------------------------------------------------------------
;; start server: if a emacs starts with server, it must be the main emacs!
;;------------------------------------------------------------------------------
(defvar rw-main-emacs-p nil
  "Whether this emacs is the main emacs?")

(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (setq rw-main-emacs-p t)
    (message "rw: success start server!"))

  ;; 如果在emacs是启动了server的时候(我保证全局只有一个server), 那么"C-x C-s"不会杀掉这个emacs,
  ;; 需要手动执行`kill-emacs', 才会杀掉最后一个emacs.
  (defun rw-save-buffers-kill-terminal (&optional arg)
  "Offer to save each buffer, then kill the current connection.
If the current frame has no client and `rw-main-emacs-p' is nil, kill Emacs itself using
`save-buffers-kill-emacs'.

With prefix ARG, silently save all file-visiting buffers, then kill.

If emacsclient was started with a list of filenames to edit, then
only these files will be asked to be saved."
  (interactive "P")
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal arg)
    ;; 只有在此emacs没有开启server时(我只会打开一个server), 杀掉它. 否则什么都不做.
    (if rw-main-emacs-p
        (message "This emacs is the MAIN emacs! You should not kill it!")
      (save-buffers-kill-emacs arg))))

  )

(provide 'init-server)
