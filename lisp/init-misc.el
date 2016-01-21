;;; modeline
(require 'init-modeline)

;;; isearch
(require 'init-isearch)

;;; save place
(require 'init-saveplace)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; TODO
(require 'init-dired) ;; only affect dired-mode
(require 'init-uniquify) ;; nicer naming buffers for files with identical names
(require 'init-ibuffer) ;; only affect ibuffer-mode





(provide 'init-misc)