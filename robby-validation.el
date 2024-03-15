;;; robby-validation.el  --- robby validation functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to validate that Chat API values are within valid ranges.
;; Used in customization and `robby-api-options' transient.

;;; Code:

(require 'robby-utils)

(defvar robby--valid-ranges
  '((chat-temperature . (0.0 . 2.0))
    (chat-top-p . (0.0 . 1.0))
    (chat-presence-penalty . (-2.0 . 2.0))
    (chat-frequency-penalty . (-2.0 . 2.0))))

(defun robby--validate (name value)
  "Validate that VALUE is within the valid range for option NAME.
Return nil if valid, or an error message if not.

VALUE can be a string or a number. NAME is a symbol that has a
key in `robby--valid-ranges'.

If INCLUDE-NAME is non-nil, include the option name in the error message."
  (let* ((range (alist-get name robby--valid-ranges))
         (min (car range))
         (max (cdr range))
         (err-msg (format "Please enter a number between %s and %s." min max)))
    ;; whoops there's no range in `robby-valid-ranges' for this option
    (when (not range)
      (error "No validation range for %s" name))

    ;; an empty string or nil is valid
    (if (or (not value) (and (stringp value) (string= value "")))
        nil
      ;; value must be a number or a string that can be converted to a number
      (if (or (symbolp value)
              (and (stringp value) (not (robby--decimal-p value))))
          err-msg
        ;; convert string to number if it's a string
        (let ((n (if (stringp value) (string-to-number value) value)))
          ;; validate that the number is within the range
          (if (or (< n min) (> n max))
              err-msg
            nil))))))

(provide 'robby-validation)

;;; robby-validation.el ends here
