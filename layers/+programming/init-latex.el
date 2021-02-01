(use-package tex-site 
  :ensure auctex
  ;; 不知道出了什么问题, 导致下面这一行用不了. 这次配置出现的问题都是功能的封装不好, 每次都要回忆起最细节的东西, 很伤.
  ;; :mode ("\\.tex\\'" . Tex-latex-mode)
  :config
  ;; 将texlive tools的目录加入variable ~exec-path~ 和环境变量 ~PATH~ 中。
  ;; 参考资料： [[https://blog.csdn.net/fenxian2011/article/details/19254949]]

  (rw/prepend-to-exec-path "C:\\texlive\\2018\\bin\\win32")

  ;; 放弃折腾preview了, 简直不能成功, 文档也很糟糕, 放弃. 
  ;; (require 'preview-latex)
  (require 'reftex)

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (use-package company-auctex
    :ensure t)
  
  ;; (defun tex-company ()
  ;;   ;; `company-math-symbols-unicode' is used to enter unicode symbols, which in not useful in latex mode. 
  ;;   (setup-company-mode '((company-math-symbols-latex
  ;;                          ;; company-math-symbols-unicode
  ;;                          company-auctex-macros
  ;;                          company-auctex-symbols
  ;;                          company-auctex-environments
  ;;                          company-dabbrev)
  ;;                         ;; company-auctex-labels
  ;;                         ;; company-auctex-bibs
  ;;                         ))
  ;;   ;; (company-auctex-init)
  ;;   )

  (defun tex-func ()
    (rainbow-delimiters-mode)
    (smartparens-mode)
    (yas-on)
    (setup-company-mode tex-mode-company-backends)
    (LaTeX-math-mode)
    (reftex-mode)
    ;; (setq TeX-command-default "LaTeX")
    ;; (local-set-key (kbd "C-c C-a"))
    )

  (add-hook 'LaTeX-mode-hook 'tex-func)
  ;; (add-hook 'TeX-mode-hook 'tex-func)
  ;; (add-hook 'plain-tex-mode-hook)


  ;;   配置用于打开pdf的软件， 这里选择SumatraPDF， 并且可以配置双击pdf会用emacs打开对应的latex代码， 很酷。

  ;; 其中对于反向打开emacs中命令行的参数还不是很理解。

  ;; 参考资料： [[http://juanjose.garciaripoll.com/blog/latex-with-emacs-on-windows]]

  (setq TeX-PDF-mode t) 

  (setq TeX-source-correlate-mode t) 

  (setq TeX-source-correlate-method 'synctex) 

  (setq TeX-view-program-list 
        '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")))) 

  (setq TeX-view-program-selection 
        '(((output-dvi style-pstricks) 
           "dvips and start") 
          (output-dvi "Yap") 
          (output-pdf "Sumatra PDF") 
          (output-html "start"))) 


  (defun pdf-viewer-config ()
    (visual-line-mode +1)
    (assq-delete-all 'output-pdf TeX-view-program-selection)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))

  (add-hook 'LaTeX-mode-hook 'pdf-viewer-config)
  
  ;; 以下是在做项目时，处理coq代码时使用的，其实不能算是配置的一部分，不应该导出。

  (defvar rw/latex-newcommand-regexp nil
    "Regexp for `\\newcommand' in latex mode.")
  (setq rw/latex-newcommand-regexp "^[\\]newcommand.*")

  (defun rw-latex-cut-all-newcommands ()
    "Cut all `\\newcommand' in the current buffer, and store them on the paste board."
    (interactive)
    (let ((init-p (point))
          (s nil))
      (goto-char (point-max))
      (while (re-search-backward rw/latex-newcommand-regexp nil t nil)
        (setq s (cons (delete-and-extract-region
                       (line-beginning-position)
                       (+ (line-end-position) 1))
                      s)))
      (goto-char init-p)
      (if (not (null s))
          (kill-new (-reduce (lambda (s1 s2) (concat s1 s2))
                             s))
        (error "No command to cut!"))
      ))

  ;; \newcommand{\SplitNewBlock}[1]{\ensuremath{\mathsf{SplitNewBlock}(#1)}}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun rw-latex-find-rref ()
    (re-search-forward "rref{\\(?2:[^[:blank:]]*\\)}") ;; the number "2" is the manually name
    (princ (match-string 2)))

  (defun rw-latex-find-equation (enumber)
    (interactive "s")
    (re-search-backward (concat "llabel{" (regexp-quote enumber) "}")) ;; must using `regexp-quote'!
    (forward-line)
    (goto-char  (line-beginning-position))
    (re-search-forward "^[[:blank:]]*\\(?1:.*\\)[[:blank:]]*[\\]*[[:blank:]]*")
    (princ (match-string 1))
    )

  (defun rw-latex-find-and-insert-equation ()
    (interactive)
    (let* ((enumber (rw-latex-find-rref))
           (p (point))
           (eqs (rw-latex-find-equation enumber)))
      (goto-char p)
      (if (and (not (string-match "begin" eqs))
               (not (looking-at ":")))
          (insert ":\\(" eqs "\\)"))
      (forward-char)
      ))
  )

(provide 'init-latex)
