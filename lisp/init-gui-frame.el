;;; modeline
(require 'init-modeline)

;;; color theme
(require 'init-color-theme)

;;; column number
(column-number-mode t)

;;; highlight balanced parenthesis
(show-paren-mode t)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; set the line space: the space between lines. Extremely important for comfortable!
(setq-default line-spacing 0.2)

;;; init startup window
(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs loves you!\n\n"))
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)






(provide 'init-gui-frame)
