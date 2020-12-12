(straight-use-package
 '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"
            :no-build t
            :fork (:host github
                         :repo "lczch/insert-translated-name")
            ))

(rw-add-to-load-path (expand-file-name "insert-translated-name" rw-straight-repos-dir))

(use-package insert-translated-name
  :init
  (evil-leader/set-key
    "tr" 'insert-translated-name-replace
    "ti" 'insert-translated-name-insert)
  :commands (insert-translated-name-insert
             insert-translated-name-replace)
  :config
  (setq insert-translated-name-default-style "origin")
  (setq insert-translated-name-line-style-mode-list '())
  (setq insert-translated-name-camel-style-mode-list '())
  (setq insert-translated-name-underline-style-mode-list '())
  )

(provide 'init-english)
