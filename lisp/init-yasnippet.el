(require 'yasnippet)

(let ((rice-wine-yas-dir (expand-file-name "snippets" rice-wine-dir)))
  (setq yas-snippet-dirs
	`(,rice-wine-yas-dir)))

(yas-reload-all)

(defun yas-on ()
  (interactive)
  (yas-minor-mode 1))

(defun yas-off ()
  (interactive)
  (yas-minor-mode 0))

(define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
(define-key evil-emacs-state-map (kbd "M-j") 'yas-expand)

(provide 'init-yasnippet)
