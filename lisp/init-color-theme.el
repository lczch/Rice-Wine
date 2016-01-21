;;; from percell's configuration
(require 'color-theme-sanityinc-solarized)
(setq-default custom-enabled-themes '(sanityinc-solarized-dark))
(load-theme 'sanityinc-solarized-dark t)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (color-theme-sanityinc-solarized-light))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (color-theme-sanityinc-solarized-dark))

(provide 'init-color-theme)