;;; robby-spinner.el  --- robby mode line spinner  -*- lexical-binding:t -*-

;;; Commentary:

;; Buffer local modeline spinner for robby

(require 'spinner)

;;; Code:

(defcustom robby-spinner 'rotating-line
  "What kind of spinner to use to show progress."
  :group 'lsp-mode
  :type `(choice :tag "Spinner type"
                 ,@(mapcar (lambda (c) (list 'const (car c)))
                           spinner-types))
  :group 'robby)

(defconst robby--lighter
  '(" robby " (:eval (spinner-print robby--spinner))))

(defvar-local robby--spinner nil)
(put 'robby--spinner 'permanent-local t)

(defun robby--create-spinner ()
  "Create a new spinner for current buffer only."
  (spinner-create robby-spinner t 5))

(defun robby--spinner-start ()
  "Start spinner for current buffer."
  (when (bound-and-true-p robby-spinner-mode)
    (setq robby--spinner (robby--create-spinner))
    (spinner-start robby--spinner)))

(defun robby--spinner-stop ()
  "Stop spinner."
  (spinner-stop robby--spinner))

(provide 'robby-spinner)

;;; robby-spinner.el ends here
