;;; ELPA
;; require package from network: I only use it to get a package from network,
;; and move it to site-lisp/, or get the description of a package.
(require 'init-elpa)

;;; configure the appearance of emacs
(require 'init-gui-frame)

;;; functions and configuration about editing text
(require 'init-editing)

;;; functions and manipulation about windows, from purcell's emacs.d
(require 'init-windows)

;;; custom-file and backup-directory
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))

;;; info mode
(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

;;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-ibuffer) ;; only affect ibuffer-mode

(provide 'init-misc)
