;; (rw-straight-use-package 'lua-mode "immerrr" "lczch")

(use-package lua-mode
  :disabled 
  ;; :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config 
  (setq lua-indent-level 2
        lua-indent-string-contents t)

  (add-hook 'lua-mode-hook 'rice-wine-prog-func)
  )

(provide 'init-lua)
