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
  :keymap robby-mode-map)

(provide 'robby-mode)

;;; robby-mode.el ends here
