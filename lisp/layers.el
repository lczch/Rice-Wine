;; 抄自以下文章，问好看起见，改了个名字。
;; 参考资料： [[https://blog.csdn.net/fenxian2011/article/details/19254949]]
(defun rw/prepend-to-exec-path (path)  
  "push the path to the emacs internal `exec-path' and \"PATH\" env variable.  
Return the updated `exec-path'"  
  (setenv "PATH" (concat (expand-file-name path)  
                         path-separator  
                         (getenv "PATH")))  
  (setq exec-path  
        (cons (expand-file-name path)  
              exec-path)))

(use-package cygwin-mount
  :config
  (cygwin-mount-activate))

(require 'init-elpa)
(require 'init-straight)
(require 'init-locales)
;; configure the appearance of emacs
(require 'init-modeline)
(require 'init-theme)
(require 'init-fonts)
;; (require 'init-isearch)
(require 'init-minibuff)
(require 'init-windows)


(require 'init-evil)
(require 'init-dired)
(require 'init-ibuffer)


(require 'init-server)
(require 'init-yasnippet)
(require 'init-org-mode)
(require 'init-company-mode)
(require 'init-ivy-mode)

(require 'init-evil-escape)
(require 'init-programming)
(require 'init-chinese-font)
(require 'init-latex)
(require 'init-eshell)
(require 'init-english)

;; Nicer naming of buffers for files with identical names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package init-info-mode
  :mode (("\\.info\\'" . info-mode)))

(use-package visual-regexp
  :commands (vr/query-replace)
  :init 
  (evil-leader/set-key
    "rr" 'vr/query-replace
    ;; "vm" 'vr/mc-mark
    ))

;; expand-region: increase selected region by semantic units
(use-package expand-region
  :config
  (evil-leader/set-key
    "xx" 'er/expand-region)
  
  (setq expand-region-contract-fast-key "z")
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  )

;; save place
(use-package saveplace
  :config
  (setq-default save-place t))

;; Highlight the cursor whenever the window scrolls
;; beacon: need package "seq"
(use-package beacon
  :config
  (beacon-mode 1))

(use-package browse-kill-ring
  :config
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
  )

;; recentf
(use-package recentf
  :config
  ;; (setq recentf-max-menu-items 50) ; 和ivy无关
  (setq recentf-max-saved-items 100)
  )

;; TODO: may switch to gtags?
(use-package init-xcscope)

(use-package init-clipboard)

(use-package which-key
  :config
  (which-key-mode 1))

;; TODO: I use this seldom.
(use-package init-emacs-w3m)

;; TODO: I use this seldom.
(use-package init-profiler)

;;------------------------------------------------------------------------------
;; about programming
;;------------------------------------------------------------------------------
(use-package init-markdown)

(use-package init-haskell-mode)

(use-package init-magit)

;; (use-package tex-mode
;;   :init
;;   (add-hook 'latex-mode-hook 'smartparens-mode)
;;   (add-hook 'latex-mode-hook 'rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
;; misc configurations
;;------------------------------------------------------------------------------
;; debug on
(global-set-key (kbd "<f12>") 'toggle-debug-on-error)

(evil-leader/set-key
  "xh" 'mark-whole-buffer
  "do" 'rw-display-current-buffer-other-frame
  "eb" 'eval-buffer
  "rb" 'revert-buffer) 

(fset 'yes-or-no-p 'y-or-n-p)
(setq history-delete-duplicates t)

;; some basic preferences
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

;; custom-file and backup-directory
(setq auto-save-interval 50)
(let ((my-custom-file (expand-file-name "custom.el" rice-wine-dir))
      (my-backup-dir (expand-file-name "backups" rice-wine-dir)))
  (setq custom-file my-custom-file)
  (setq backup-directory-alist `(("." . ,my-backup-dir))))

;; about Semantic
(setq semanticdb-default-save-directory nil)

;; (global-set-key (kbd "<f5>")
;;                 #'(lambda ()
;;                     (interactive)
;;                     (semantic-grammar-create-package)
;;                     (eval-buffer)))

;; (global-set-key (kbd "<f6>")
;;                 #'(lambda ()
;;                     (interactive)
;;                     (revert-buffer nil t)
;;                     (bovinate)))

;;------------------------------------------------------------------------------
;; restore desktop
;;------------------------------------------------------------------------------
;; (when rw-main-emacs-p
;;   (use-package init-desktop))

;;------------------------------------------------------------------------------
;; printer: we need to install "xpp" through os package manager
;;------------------------------------------------------------------------------
(setq lpr-command "xpp")


(provide 'layers)
