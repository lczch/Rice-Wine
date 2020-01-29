;;; change-log
;; 2020/1/29: install racer 

;;; TODO
;; 我觉得这两个包应该可以用melpa的, 不用从git hub上下. 

(rw-straight-use-package 'rust-mode "rust-lang" "lczch")
(rw-straight-use-package 'cargo.el "kwrooijen" "lczch")
;; flycheck下次再配
;; (rw-straight-use-package 'flycheck "flycheck" "lczch")
;; (rw-straight-use-package 'flycheck-rust "flycheck" "lczch")

(use-package rust-mode
  :init
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :defer t
  :config
  (use-package cargo)

  (use-package racer
    :ensure t)

  (defun rust-mode-func ()
    (smartparens-mode 1)
    (rainbow-delimiters-mode 1)
    (setup-company-mode '((company-capf company-tabnine)))
    (setq-local company-tooltip-align-annotations t)
    (cargo-minor-mode 1)
    (racer-mode 1)
    (eldoc-mode 1)
    )
  
  (add-hook 'rust-mode-hook 'rust-mode-func)
  )

(provide 'init-rust)
