;;; isearch
(require 'init-isearch)

;;; expand-region: increase selected region by semantic units
(require 'init-expand-region)

;;; save place
(require 'init-saveplace)

;;; Highlight the cursor whenever the window scrolls
;; beacon: need package "seq"
(require 'beacon)
(beacon-mode 1)

;;; browse-kill-ring
(require 'browse-kill-ring)
;; no duplicates
(setq browse-kill-ring-display-duplicates nil)
;; preview is annoying
(setq browse-kill-ring-show-preview nil)
(browse-kill-ring-default-keybindings)
(define-key evil-normal-state-map (kbd "M-y") 'browse-kill-ring)
;; hotkeys:
;; n/p => next/previous
;; s/r => search
;; l => filter with regex
;; g => update/refresh

(require 'init-uniquify) ;; nicer naming buffers for files with identical names

(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'display-buffer-other-frame)

(global-set-key (kbd "C-c o") 'other-frame)

;;------------------------------------------------------------------------------
;; some basic preferences
;;------------------------------------------------------------------------------
(setq-default buffers-menu-max-size 30
              case-fold-search t
              save-interprogram-paste-before-kill t
              indent-tabs-mode nil
              mouse-yank-at-point t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)


(provide 'init-editing)
