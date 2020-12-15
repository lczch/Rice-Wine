;; highlight pairs 
(use-package rainbow-delimiters
  ;; :commands (rainbow-delimiters-mode)
  :config
  (define-globalized-minor-mode rainbow-delimiters-global-mode
    rainbow-delimiters-mode
    rainbow-delimiters-mode)
  ;; active rainbow-delimiters minor mode globally 
  (rainbow-delimiters-global-mode))

;; smartparens
(use-package smartparens
  ;; :init
  ;; (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  ;; :commands (smartparens-mode smartparens-strict-mode turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (setq sp-autoskip-closing-pair 'always)
  (sp-use-smartparens-bindings)

  (sp-with-modes 'tuareg-mode
    ;; disable auto insert of "'" 
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  
  ;; Don't pair lifetime specifiers
  (sp-local-pair 'rust-mode "'" nil :actions nil)

  (sp-with-modes 'minibuffer-inactive-mode
    (sp-local-pair "'" nil :actions nil))

  ;; add newline and indent when enter {|}
  (sp-with-modes
      '(c-mode rust-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

  ;; turn smartparens globally
  (smartparens-global-mode)
  )

(provide 'init-pairs)
