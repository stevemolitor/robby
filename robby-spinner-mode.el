;;; robby-spinner-mode.el  --- robby minor mode  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-spinner-mode' minor mode.

(require 'spinner)
(require 'robby-spinner)

;;; Code:

;;;###autoload
(define-minor-mode robby-spinner-mode
  "Minor mode for robby commands."
  :global t
  :lighter robby--lighter
  :keymap robby-spinner-mode-map
  ;; autoload built in command, robby transient when entering robby-spinner-mode
  (require 'robby-commands)
  (require 'robby-transients))

(provide 'robby-spinner-mode)

;;; robby-spinner-mode.el ends here
