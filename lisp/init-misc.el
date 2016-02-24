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

;;; frame-hooks
(require 'init-frame-hooks)

;;; fonts
(require 'init-fonts)

;;; linum mode on
(require 'init-linum-mode)

;;; locales
(require 'init-locales)

;;; markdown-mode
(require 'init-markdown)

(fset 'yes-or-no-p 'y-or-n-p)
(setq history-delete-duplicates t)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; increase and decrease font size in GUI emacs
(when (display-graphic-p)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

;; sh
(require 'init-sh)

;; writting
(require 'init-writting)

;;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-ibuffer) ;; only affect ibuffer-mode

(provide 'init-misc)
