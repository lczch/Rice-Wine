(use-package profiler
  :init
  (evil-leader/set-key
    "ps" 'rw-profiler-toggle
    "pr" 'profiler-report)

  :commands (rw-profiler-toggle)
  :config 
  (defun rw-profiler-toggle ()
    "Turn on/off profiler."
    (interactive)
    (if (not (profiler-running-p))
        (profiler-start 'cpu)
      (rw-profiler-stop)))
  )

(provide 'init-profiler)
