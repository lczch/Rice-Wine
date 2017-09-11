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
    (setup-company-mode '((company-math-symbols-latex
                           company-math-symbols-unicode
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
  )

(provide 'init-latex)
