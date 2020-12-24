;; (rw-straight-use-package 'swiper "abo-abo" "lczch")

;; (rw-straight-use-package 'amx "DarwinAwardWinner" "lczch")

(use-package ivy
  :ensure t
  :config
  (use-package amx
    :ensure t)
  
  (use-package swiper
    :ensure t)
  
  (use-package counsel
    :ensure t
    :config
    (when (executable-find "rg")
      ;; ripgrep says that "-n" is enabled actually not,
      ;; so we manually add it
      (setq counsel-grep-base-command
            (concat (executable-find "rg")
                    " -n -M 512 --no-heading --color never -i \"%s\" %s")))
    
    ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
    ;; press "M-o" to choose ivy action
    (ivy-set-actions
     'counsel-find-file
     '(("j" find-file-other-frame "other frame")
       ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
       ("x" counsel-find-file-extern "open externally")
       ("d" delete-file "delete")
       ("r" counsel-find-file-as-root "open as root"))))

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

  
  ;; {{  C-o f to toggle case sensitive, @see https://github.com/abo-abo/swiper/issues/1104
  (defun re-builder-extended-pattern (str)
    "Build regex compatible with pinyin from STR."
    (let* ((len (length str)))
      (cond
       ;; do nothing
       ((<= (length str) 1))

       ;; If the first character of input in ivy is ":",
       ;; remaining input is converted into Chinese pinyin regex.
       ((string= (substring str 0 1) ":")
        (setq str (my-pinyinlib-build-regexp-string (substring str 1 len))))

       ;; If the first character of input in ivy is "/",
       ;; remaining input is converted to pattern to search camel case word
       ;; For example, input "/ic" match "isController" or "isCollapsed"
       ((string= (substring str 0 1) "/")
        (let* ((rlt "")
               (i 0)
               (subs (substring str 1 len))
               c)
          (when (> len 2)
            (setq subs (upcase subs))
            (while (< i (length subs))
              (setq c (elt subs i))
              (setq rlt (concat rlt (cond
                                     ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                      (format "%c" c))
                                     (t
                                      (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                                (format "%c" c))
                                              "[a-z]+")))))
              (setq i (1+ i))))
          (setq str rlt))))
      (ivy--regex-plus str)))
  ;; }}
  
  ;; ivy--regex-plus: can use "!" to exclude some keywords. 
  (setq ivy-re-builders-alist '((t . re-builder-extended-pattern)))
  ;; (setq ivy-re-builders-alist
  ;;       '((swiper . ivy--regex-plus)
  ;;         ;; fuzzy search in counsel-rg is noisy. 
  ;;         (counsel-rg . ivy--regex-plus)
  ;;         ;; fuzzy is not suitable for me!
  ;;         ;; (t      . ivy--regex-fuzzy)
  ;;         (t . ivy-regex-plus)
  ;;         ))
  
  (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "<f2>") 'counsel-imenu)
  ;; (global-set-key (kbd "<f3> i") 'counsel-info-lookup-symbol)
  ;; (global-set-kqey (kbd "<f3> u") 'counsel-unicode-char)
  
  ;; better performance on everything (especially windows), ivy-0.10.0 required
  ;; @see https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
  
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
