(use-package profiler
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
