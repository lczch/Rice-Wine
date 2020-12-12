(use-package caml
  :defer t
  ;; tuareg和merlin都需要这个package
  )

(use-package merlin
  :load-path
  (lambda ()
    (expand-file-name "emacs/site-lisp"
                      (car (process-lines "opam" "config" "var" "share"))))
  :commands (merlin-mode)
  :config
  ;; ocamlmerlin不会系统PATH中, 需手动指定
  ;; 现在使用的是4.02.3版本的merlin
  (setq merlin-command 
        (expand-file-name "ocamlmerlin"
                          (car (process-lines "opam" "config" "var" "bin"))))
  ;; (setq merlin-command "/usr/local/bin/ocamlmerlin")
  ;; process-lines, 又学会了新姿势
  )

(use-package tuareg
  :mode (("\\.\\(ml\\|mli\\|mly\\|mll\\|mlp\\)\\'" . tuareg-mode))
  :commands (tuareg-mode)
  :config

  (use-package merlin-company)
  
  (defvar tuareg-company-backends
    '(merlin-company-backend company-bbdb))
  
  (defun tuareg-mode-func ()
    (rice-wine-prog-func)
    (merlin-mode)
    (yas-minor-mode)
    (setup-company-mode tuareg-company-backends)
    )

  ;; 这也太厉害了!! ocaml社区的人怎么能如此热心
  ;; 这是在官网上复制的, 只是改了个名字
  (defun rw-opam-update-env ()
    "Update system variables according `opam config env'"
    (interactive)
    (message "Run rw-opam-update-env!")
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var))))

  ;; 将`rw-opam-update-env'加在`tuareg-run-ocaml'的前面, 这样每次运行ocaml之前都会自动更新
  ;; (add-function :before (symbol-function 'tuareg-run-process-if-needed)
  ;;               'rw-opam-update-env)
  ;; 这样会出现莫名其妙的问题, 现在运行ocaml我会打开一个新的emacs.
  ;; 好吧, 已经测试出来了, 是这个advice会莫名的不运行, 真是奇怪的行为.
  ;; 已经能运行了, 但还是不对. 启动的是ocaml 4.02.3,用的lib是4.01.0, 我想达到的目的的运行ocaml 4.01.0, 不知道tuareg的代码里是怎么写这个调用外部函数的, 有点怪异.
  (setq tuareg-use-smie nil) ;; 不关SMIE的缩进, 秒遇bug... 而且还要用老版本, 2.0.8
  (add-hook 'tuareg-mode-hook 'tuareg-mode-func)
  )

(provide 'init-ocaml)
