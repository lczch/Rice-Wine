;; 用快速输入"kj"来做escape, 真是天才的想法.

;; (straight-use-package
;;  '(evil-escape :type git :host github :repo "syl20bnr/evil-escape"
;;                :no-build t
;;                :fork (:host github
;;                             :repo "lczch/evil-escape")
;;             ))

;; (rw-add-to-load-path (expand-file-name "evil-escape" rw-straight-repos-dir))

(use-package evil-escape
  :disabled
  :ensure t
  :config 
  ;; {{ https://github.com/syl20bnr/evil-escape
  (setq-defxault evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (setq-default evil-escape-key-sequence "kj")
  ;; disable evil-escape when input method is on
  (evil-escape-mode 1)
  ;; }}
  )

(provide 'init-evil-escape)
