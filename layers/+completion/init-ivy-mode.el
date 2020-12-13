;; (rw-straight-use-package 'swiper "abo-abo" "lczch")

;; (rw-straight-use-package 'amx "DarwinAwardWinner" "lczch")

(use-package ivy
  :ensure t
  :config
  (use-package amx
    :ensure t)
  
  (use-package swiper
    :ensure t
    :config
    (evil-leader/set-key
      "ss" 'swiper
      "sb" 'swiper-all)
    )
  
  (use-package counsel
    :ensure t
    :config
    ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
    ;; press "M-o" to choose ivy action
    (ivy-set-actions
     'counsel-find-file
     '(("j" find-file-other-frame "other frame")
       ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
       ("x" counsel-find-file-extern "open externally")
       ("d" delete-file "delete")
       ("r" counsel-find-file-as-root "open as root"))))

  (use-package projectile
    :ensure t
    :config
    (use-package counsel-projectile
      :ensure t
      :config
      (setq projectile-switch-project-action 'counsel-projectile-find-file)
      (evil-leader/set-key
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file
        "pr"    'projectile-recentf))

    (setq projectile-completion-system 'ivy)
    (projectile-mode +1)
    )
 
  ;;; make ivy bebavior similarly with ido
   ;; Don't open directory mode: https://github.com/abo-abo/swiper/wiki/Dont-open-directory-mode
  (setq ivy-extra-directories nil)
  
  (defun eh-ivy-open-current-typed-path ()
    (interactive)
    (when ivy--directory
      (let* ((dir ivy--directory)
             (text-typed ivy-text)
             (path (concat dir text-typed)))
        (delete-minibuffer-contents)
        (ivy--done path))))

  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-f") 'eh-ivy-open-current-typed-path)
  ;; I can enter endless TAB without select anything!
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)

  (evil-leader/set-key
    ;; files
    "ff"  'counsel-find-file
    "fL"  'counsel-locate
    ;; help
    "?"   'counsel-descbinds
    "hdf" 'counsel-describe-function
    "hdv" 'counsel-describe-variable
    ;; insert
    "iu"  'counsel-unicode-char
    ;; jump
    ;; register/ring
    "ry"  'counsel-yank-pop
    ;; jumping
    "sj"  'counsel-imenu
    ;; themes
    "Ts"  'counsel-load-theme
    )
  
  ;; hide dired buffers, from ivy's wiki in github.
  (defun d/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

  (add-to-list 'ivy-ignore-buffers #'d/ignore-dired-buffers)

  
  
  ;; not good experience
  ;; (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)

  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ;; work around ivy issue.
  ;; @see https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy)

  ;; use fuzzy match
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "<f2>") 'counsel-imenu)
  ;; (global-set-key (kbd "<f3> i") 'counsel-info-lookup-symbol)
  ;; (global-set-kqey (kbd "<f3> u") 'counsel-unicode-char)
  
  ;; Press C-p and Enter to select current input as candidate
  ;; https://oremacs.com/2017/11/30/ivy-0.10.0/
  (setq ivy-use-selectable-prompt t)

  (defun ivy-occur-grep-mode-hook-setup ()
    ;; no syntax highlight, I only care performance when searching/replacing
    (font-lock-mode -1)
    ;; @see https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
    (column-number-mode -1)
    ;; turn on wgrep right now
    ;; (ivy-wgrep-change-to-wgrep-mode) ; doesn't work, don't know why
    (local-set-key (kbd "RET") #'ivy-occur-press-and-switch)
    )
  (add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-grep-mode-hook-setup)

  (ivy-mode 1) ; it enables ivy UI for `kill-buffer'
  )

(provide 'init-ivy-mode)
