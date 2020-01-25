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

  (defun rust-mode-func ()
    (smartparens-mode 1)
    (rainbow-delimiters-mode 1)
    (cargo-minor-mode 1)
    )
  
  (add-hook 'rust-mode-hook 'rust-mode-func)
  )

(provide 'init-rust)
