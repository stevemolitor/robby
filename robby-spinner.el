;;; robby-spinner.el  --- robby mode line spinner  -*- lexical-binding:t -*-

;;; Commentary:

;; Buffer local modeline spinner for robby, robby-spinner-mode

;;; Code:

(require 'spinner)

(require 'robby-customization)

(defvar-local robby--spinner nil)
(put 'robby--spinner 'permanent-local t)

(defun robby--create-spinner ()
  "Create a new spinner for current buffer only."
  (spinner-create robby-spinner t 5))

(defun robby--spinner-start ()
  "Start spinner for current buffer."
  (when (bound-and-true-p robby-mode)
    (setq robby--spinner (robby--create-spinner))
    (spinner-start robby--spinner)))

(defun robby--spinner-stop ()
  "Stop spinner."
  (spinner-stop robby--spinner)
  (setq robby--spinner nil))

(defvar robby--spinner-lighter '(:eval (spinner-print robby--spinner)))

(defun robby-spinner-modeline ()
  "Return spinner modeline.

Use in a custom modeline format like this:

    (:eval (robby-spinner-modeline))
."
  (format robby-spinner-lighter-format
          (if robby--spinner
              (spinner-print robby--spinner)
            "")))

(provide 'robby-spinner)

;;; robby-spinner.el ends here
