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


;; (require 'init-evil)
;; (require 'init-evil-escape)
(require 'init-dired)
(require 'init-ibuffer)


(require 'init-server)
(require 'init-yasnippet)
(require 'init-org-mode)
(require 'init-company-mode)
(require 'init-ivy-mode)

(require 'init-pairs)

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
  :commands (vr/query-replace))

;; expand-region: increase selected region by semantic units
;; (use-package expand-region
;;   :config
;;   (setq expand-region-contract-fast-key "z")
;;   (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
;;   )

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
  ;; (define-key evil-normal-state-map (kbd "M-y") 'browse-kill-ring)
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

(use-package so-long
  :ensure t
  :config (global-so-long-mode 1))

;; jump to a tag. use my git version, for there is a bug on windows. 
(use-package counsel-etags
  ;; :commands (counsel-etags-find-tag-at-point
  ;;            counsel-etags-virtual-update-tags)
  :config
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
                    'counsel-etags-virtual-update-tags 'append 'local)))
  
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories)

  ;; counsel-etags-ignore-directories does NOT support wildcast
  (push "build_clang" counsel-etags-ignore-directories)
  (push "build_clang" counsel-etags-ignore-directories)
  ;; counsel-etags-ignore-filenames supports wildcast
  (push "TAGS" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames)

  
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Do case-sensitive tag searches
  (setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)

  ;; list tags by imenu.
  (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

  (when (eq system-type 'windows-nt)
    (setq counsel-etags-ctags-program "C:\\\\msys64\\\\home\\\\lzh\\\\bin\\\\ctags")
    (setq counsel-etags-grep-program "C:\\\\Users\\\\lzh\\\\.cargo\\\\bin\\\\rg"))

  ;; don't ignore files in ".gitignore": need to search for installed packages. 
  ;; (setq counsel-etags-ignore-config-files nil)
  )

;; locate a file -- replace "rgrep"
;; parameters for "find": find . -type f -name '*keyword*'
(use-package grep-dired
  :commands (grep-dired-dwim grep-dired))

;; use `find-file-in-project' instead of `projectile'.
(use-package find-file-in-project
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    ;; (setq ffip-find-executable "c:\\\\msys64\\\\usr\\\\bin\\\\find")
    (setq ffip-find-executable "C:\\\\Users\\\\lzh\\\\.cargo\\\\bin\\\\fd"))

  (setq ffip-use-rust-fd t)
  )

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :ensure t 
    :config
    (setq rg-group-result t
          rg-show-columns t)

    ;; (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (rg-enable-default-bindings)))

;; pinyin
(use-package pinyinlib
  :ensure t
  :commands (pinyinlib-build-regexp-char
             pinyinlib-build-regexp-string
             my-pinyinlib-build-regexp-string)
  :config
  ;; from redguardtoo
  (defun my-pinyinlib-build-regexp-string (str)
    "Build pinyin regexp from STR."
    (let* (rlt (i 0) ch)
      (while (< i (length str))
        (setq ch (elt str i))
        (setq rlt (concat rlt
                          (cond
                           ((and (<= ?a ch) (<= ch ?z))
                            (pinyinlib-build-regexp-char ch))
                           (t
                            (char-to-string ch)))))
        (setq i (1+ i)))
      rlt))
  )


(use-package init-keyfreq)
;; color-rg: replace `rgrep'
;; there exists a error. 
;; (use-package color-rg)
;;------------------------------------------------------------------------------
;; misc configurations
;;------------------------------------------------------------------------------

(setq ring-bell-function 'ignore)
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


;; read global key-bindings
(use-package init-keybindings)

;; replace evil-mode
(use-package init-meow)


(provide 'layers)
