;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package elpy
  :init
  (with-eval-after-load 'python
    ;; run command `pip install jedi flake8 importmagic` in shell,
    ;; or just check https://github.com/jorgenschaefer/elpy
    (unless (or (is-buffer-file-temp)
                (not buffer-file-name)
                ;; embed python code in org file
                (string= (file-name-extension buffer-file-name) "org"))
      (elpy-enable))
    ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
    ;; emacs 24.4+
    (setq electric-indent-chars (delq ?: electric-indent-chars)))
  :ensure t
  )

(provide 'init-python)
