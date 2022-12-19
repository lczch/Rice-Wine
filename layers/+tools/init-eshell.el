;;; TODO
;; 输入"&"会花费很长时间反应. 没找到原因.

;; (straight-use-package
;;  '(aweshell :type git :host github :repo "manateelazycat/aweshell"
;;             :no-build t
;;             :fork (:host github
;;                          :repo "lczch/aweshell")
;;             ))

;; (rw-add-to-load-path (expand-file-name "aweshell" rw-straight-repos-dir))

(use-package aweshell
  :init
  (autoload 'aweshell-new "aweshell" "Awesome Eshell" t nil)
  (autoload 'aweshell-toggle "aweshell" "Awesome Eshell" t nil)
  ;; 定义快捷键F1是召唤出shell, F5是创建一个shell
  :defer t 
  :config
  (setq eshell-prefer-lisp-functions t)
  ;; (defalias 'ec 'find-file-other-frame)
  (use-package em-alias
    :config
    ;; I find functions in emacs is more useful.
    (defalias 'ec 'find-file-other-frame)
    ;; (eshell/alias "ec" "emacsclient -c $1")
    ;; Commands about git need magic, which I am planning to use.
    (eshell/alias "gis" "git status")
    (eshell/alias "rw-git-add-all" "git add -A")
    (eshell/alias "rw-git-quick-push" "git commit -m \"1\"")
    )
  
  (defun eshell-mode-func ()
    ;; (smartparens-strict-mode 1)

    ;; `company-capf'在括号内不能启动补全, 那我还要它干么?
    ;; 看来backend是要看过代码,自己修改后才能使用的. 费劲.
    ;; 2020/1/29, capf简直有毒, 输入"&"卡住了
    ;; (company-mode 1)
    ;; (setq-local company-minimum-prefix-length 2)
    ;; (setq-local company-idle-delay 0)
    ;; (setq-local company-backends '(company-capf))

    ;; 这样的改键方式必须要加入到hook中才会生效, 好久不配, 忘记了.
    (define-key eshell-mode-map (kbd "M-p") 'aweshell-prev)
    (define-key eshell-mode-map (kbd "M-n") 'aweshell-next)
    )
  
  (add-hook 'eshell-mode-hook 'eshell-mode-func)


  ;; (define-key eshell-mode-map (kbd "M-p") 'aweshell-prev)
  ;; (eshell/alias "econf" (concat "emacsclient -c "
  ;;                               (expand-file-name "README.org" rice-wine-dir)
  ;;                               " -e (org-babel-goto-named-src-block $1)"))
  )

(provide 'init-eshell)
