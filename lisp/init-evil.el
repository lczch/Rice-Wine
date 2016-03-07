;;; evil-leader:
;; Bound frequently used commands to a short string.
;; For example, for line like `"ef" 'end-of-defun`
;;   You can either press `,ef` or `M-x end-of-defun` to execute it

;; (setq evil-leader/in-all-states t)
(require 'evil-leader)

(global-evil-leader-mode)

(setq evil-leader/leader ",")

(evil-leader/set-key
  "xb" 'switch-to-buffer)
;;; Package need
;; main package: evil
;; dependent package: undo-tree.el
(rice-wine/add-to-load-path (expand-file-name "evil/lib" rice-wine-package-dir))
(require 'undo-tree)

(require 'evil)
(evil-mode 1)

;;; Move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back t)

;;; modify evil-insert-state-map to suit my habit
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

;;; modify evil-normal-state-map to suit my habit
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-k") 'kill-line)

;;;
;; (defun rw/evil-change-read-only-file-initial-state ()
;;   (when (and  buffer-read-only
;;               (eq major-mode 'fundermental-mode))
;;     (evil-insert)))

;; (add-hook ')

(define-key evil-normal-state-map "q" nil)

;;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))


(provide 'init-evil)
