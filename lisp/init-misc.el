;;; modeline
(require 'init-modeline)

;;; isearch
(require 'init-isearch)

;;; save place
(require 'init-saveplace)

;;; window numbering: move focus between sub-windows
(require 'init-window-numbering)

;;; Highlight the cursor whenever the window scrolls
;; beacon: need package "seq"
(require 'beacon)
(beacon-mode 1)

;;; column number
(column-number-mode t)

;;; highlight balanced parenthesis
(show-paren-mode t)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; custom-file and backup-directory
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))


;;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-uniquify) ;; nicer naming buffers for files with identical names
(require 'init-ibuffer) ;; only affect ibuffer-mode

(provide 'init-misc)