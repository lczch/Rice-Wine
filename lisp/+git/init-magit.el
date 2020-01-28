(use-package magit
  :ensure t
  :init
  ;; 从melpa上安装的目录结构和git上的目录结构有差别.
  ;; 这个autoloads没有provide feature, 所以只能这样load
  (load-library "magit-autoloads")
  ;; (load-file (expand-file-name "magit/magit-autoloads.el"
  ;;                              rice-wine-package-temp-dir))

  ;; (with-eval-after-load 'info
  ;;   (info-initialize)
  ;;   (add-to-list 'Info-directory-list
  ;;                (expand-file-name "magit/Documentation" rice-wine-package-temp-dir)))
  :config
  ;; from redguardtoo
  (ivy-mode 1)
  ;; from space
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  ;; On Windows, we must use Git GUI to enter username and password
  ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
  (when (eq window-system 'w32)
    (setenv "GIT_ASKPASS" "git-gui--askpass"))
  ;; key bindings
  (evil-leader/set-key
   ;; "gb"  'spacemacs/git-blame-micro-state
   "gfh" 'magit-log-buffer-file
   "gm"  'magit-dispatch-popup
   "gs"  'magit-status
   "gS"  'magit-stage-file
   "gU"  'magit-unstage-file))

(provide 'init-magit)
