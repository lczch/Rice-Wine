;; highlight pairs 
(use-package rainbow-delimiters
  ;; :commands (rainbow-delimiters-mode)
  :config
  (define-globalized-minor-mode rainbow-delimiters-global-mode
    rainbow-delimiters-mode
    rainbow-delimiters-mode))

;; smartparens
(use-package smartparens
  :ensure t
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

  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat
                               "wrap-with-"
                               (prin1-to-string key)
                               "s"))
                    (&optional arg)
                  (interactive "p")
                  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))

  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("M-<right>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ;; ("C-c C-c ("  . wrap-with-parens)
   ;; ("C-c C-c ["  . wrap-with-brackets)
   ;; ("C-c C-c {"  . wrap-with-braces)
   ;; ("C-c C-c '"  . wrap-with-single-quotes)
   ;; ("C-c C-c \"" . wrap-with-double-quotes)
   ;; ("C-c C-c _"  . wrap-with-underscores)
   ;; ("C-c C-c `"  . wrap-with-back-quotes)
   )

  (setq sp-ignore-modes-list
        (append sp-ignore-modes-list
                '(org-mode
                  fundamental-mode
                  info-mode)))
  ;; turn smartparens globally
  (smartparens-global-mode)
  )

(provide 'init-pairs)
