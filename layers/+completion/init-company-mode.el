;;; change log:
;;
;; 2020/1/29
;;      * install tabnine: a cpmpletion backends based on deep learning. 

;;; TODO:
;; 以后还可以试一试tabnine, 大概感觉像是dabbrev的超级升级版.
;; 话说我是不是应该把company的架构改一下, 改成全局开启, 然后配置一个大多数mode都能用的backends, 而不要每次都要手动. 我看的配置好像全部都是全开的. 可能有道理.
;; 现在会自动下载company-mode(因为依赖), 但不会被使用

(use-package company
  :config
  (setq company-auto-complete nil)
  (setq company-require-match nil)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  ;; 原来一直是3, 我觉得2可能更合理一些.
  ;; update: 2020/1/29 2不行, 因为我习惯在每个单词后输入一个空格, 而2会导致空格都会引发补全
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  ;; If I actually get the point, this variable `company-begin-commands` controls
  ;; what commands of emacs can triger the starting of company.
  ;; `self-insert-command` means typing IO.
  ;; (setq company-begin-commands '(self-insert-command))
  ;; 用spacemacs的设置
  (setq company-idle-delay 0.2)

  ;; 其中n和p在slime中是有的, 我都习惯了
  (let ((map company-active-map))
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)
    (define-key map (kbd "C-/")   'company-search-candidates)
    (define-key map (kbd "C-M-/") 'company-filter-candidates)
    (define-key map (kbd "C-d")   'company-show-doc-buffer))


  ;; NOT to load company-mode for certain major modes.
  ;; Ironic that I suggested this feature but I totally forgot it
  ;; until two years later.
  ;; https://github.com/company-mode/company-mode/issues/29
  ;; (setq company-global-modes
  ;;       '(not
  ;;         eshell-mode
  ;;         comint-mode
  ;;         erc-mode
  ;;         gud-mode
  ;;         rcirc-mode
  ;;         minibuffer-inactive-mode))
  
  (use-package company-statistics
    :commands (company-statistics-mode))
  (use-package company-elisp
    :commands (company-elisp))
  (use-package company-capf
    :commands (company-capf))
  (use-package company-files
    :commands (company-files))
  (use-package company-dabbrev
    :commands (company-dabbrev))
  (use-package company-math
    :commands (company-math-symbols-latex
               company-math-symbols-unicode))
  ;; (use-package company-tabnine 
  ;;   :ensure t
  ;;   :commands (company-tabnine))
  ;; Recommended from Chen Bing. 
  (use-package company-ctags
    :ensure t
    :commands (company-ctags))
  
  ;; Customize company backends.
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-gtags company-backends))
  (setq company-backends (delete 'company-etags company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (setq company-backends (delete 'company-ropemacs company-backends))
  (push 'company-cmake company-backends)
  (push 'company-c-headers company-backends)
  (push 'company-ctags company-backends)
  (add-to-list 'company-backends 'company-files)
  ;; tabnine on windows is slow. 
  ;; (add-to-list 'company-backends #'company-tabnine) 
  ;; Specific backends for various modes: 

  (defvar elisp-company-backends
    '(company-elisp
      company-ctags
      ;; (company-elisp company-tabnine)
      ;; 在笔记本上用时很慢, 不知道为什么...
      ;; company-files
      ))
  
  (defvar slime-company-backends
    '(company-slime company-files))

  (defvar geiser-company-backends
    '(company-capf)
    ;; '(company-capf company-dabbrev)
    ;; very slow
    )

  (defvar clojure-company-backends
    '(company-capf company-files))

  (defvar cc-mode-company-backends
    '(company-ctags
      ;; company-clang
      company-dabbrev))

  (defvar tex-mode-company-backends
    ;; `company-math-symbols-unicode' is used to enter unicode symbols, which in not useful in latex mode. 
    '((company-math-symbols-latex
       ;; company-math-symbols-unicode
       company-auctex-macros
       company-auctex-symbols
       company-auctex-environments
       company-dabbrev)
      ;; company-auctex-labels
      ;; company-auctex-bibs
      ))

  ;; use company-statistics to arrange the order of candidates, show more probably selected one to the first
  ;; 这其实是个没什么用的函数, 还不如直接手写. 每次我都要想backends的语法是什么, 创造了一个DSL, 找事.
  (defun setup-company-mode (&optional backends)
    "turn-on company-mode, then make variable company-backends to buffer local, and set it to BACKENDS. If BACKENDS is nil, then `company-backends' keeps its original value. 
     Example: for elisp, (setup-company-mode '(company-elisp))"
    (company-mode 1)
    (company-statistics-mode)
    (make-local-variable 'company-backends)
    (unless (null backends)
      (setq company-backends backends)))
  )


(provide 'init-company-mode)

;;; useful company-backend
;;  company-c-headers
;;  company-elisp
;;  company-bbdb ;; BBDB stands for The Insidious Big Brother Database – an address book that you can hook into your mail- and newsreader, sync with your mobile device, etc.
;;  company-nxml
;;  company-css
;;  company-eclim
;;  company-semantic ;; completion backend using CEDET Semantic
;;  company-clang
;;  company-xcode
;;  company-cmake
;;  company-capf
;;  (company-dabbrev-code company-gtags company-etags company-keywords)
;;  company-oddmuse
;;  company-files
;;  company-dabbrev ;; this is very useful!
