(use-package profiler
  :init
  (defvar rw-profiler-state nil
    "indicate the state of profiler:
     nil: profiler is off
     t  : profiler is on")

  (evil-leader/set-key
    "pt" 'rw-profiler-toggle)

  :commands (rw-profiler-toggle)
  :config 
  (defun rw-profiler-start (mode)
    "rice-wine's profiler-start"
    (profiler-start mode)
    (setq rw-profiler-state t))

  (defun rw-profiler-stop ()
    "rice-wine's profiler-stop"
    (profiler-stop)
    (setq rw-profiler-state nil))

  (defun rw-profiler-toggle ()
    "profiler toggle: 
     if profiler is already on, then report the result and stop it.
     otherwise, turn it on.
   in this version, automatcially using cpu+mem mode"
    (interactive)
    (if (not rw-profiler-state) (rw-profiler-start 'cpu)
      (profiler-report)
      (rw-profiler-stop))))

(provide 'init-profiler)
