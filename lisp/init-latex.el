;; auctex
(use-package tex-site
  ;; 不知道出了什么问题, 导致下面这一行用不了. 这次配置出现的问题都是功能的封装不好, 每次都要回忆起最细节的东西, 很伤.
  ;; :mode ("\\.tex\\'" . Tex-latex-mode)
  :config
  (use-package preview-latex)

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (use-package company-auctex)
  (use-package reftex)
  
  (defun tex-company ()
    ;; `company-math-symbols-unicode' is used to enter unicode symbols, which in not useful in latex mode. 
    (setup-company-mode '((company-math-symbols-latex
                           ;; company-math-symbols-unicode
                           company-auctex-macros
                           company-auctex-symbols
                           company-auctex-environments
                           company-dabbrev)
                          ;; company-auctex-labels
                          ;; company-auctex-bibs
                          ))
    ;; (company-auctex-init)
    )

  (defun tex-func ()
    (rainbow-delimiters-mode)
    (smartparens-mode)
    (yas-on)
    (tex-company)
    (LaTeX-math-mode)
    (reftex-mode)
    ;; (setq TeX-command-default "LaTeX")
    ;; (local-set-key (kbd "C-c C-a"))
    )

  (add-hook 'LaTeX-mode-hook 'tex-func)
  ;; (add-hook 'TeX-mode-hook 'tex-func)
  ;; (add-hook 'plain-tex-mode-hook)

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
      (insert ":\\(" eqs "\\)")
      (forward-char)
      ))
  )

(provide 'init-latex)
