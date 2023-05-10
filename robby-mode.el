;;; robby-mode.el  --- robby minor mode  -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-mode' minor mode.

(require 'spinner)
(require 'robby-spinner)
(require 'robby-keymap)

;;; Code:

;;;###autoload
(define-minor-mode robby-mode
  "Minor mode for robby commands."
  :global t
  :lighter robby--lighter
  :keymap robby-mode-map
  ;; autoload built in command, robby transient when entering robby-mode
  (require 'robby-commands)
  (require 'robby-transients))

(provide 'robby-mode)

;;; robby-mode.el ends here
