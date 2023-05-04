;;; robby-utils.el  --- robby utility functions -*- lexical-binding:t -*-

;;; Commentary:

;; Robby utility functions.

(require 'cl-macs)

;;; Code:

(defun robby--format-message-text (text)
  "Replace % with %% in TEXT to avoid format string errors calling `message."
  (replace-regexp-in-string "%" "%%" text))

(defun robby--plist-to-alist (plist)
  "Convert PLIST to an association list (alist)."
  (cl-loop for (key value . rest) on plist by 'cddr
        collect
        (cons key value)))

(defun robby--kebab-to-snake-case (string)
  "Transform STRING from kebab to snake case.
For example \"a-b-c\" becomes a_b_c."
  (replace-regexp-in-string "-" "_" string))

(defun robby--snake-to-space-case (string)
  "Transform STRING from snake case to string with spaces.
For example \"a_b_c\" becomes \"a b c\""
  (replace-regexp-in-string "_" " " string))

(defun robby--get-region (&optional buffer)
  "Get selected region or bounds of entire buffer if no selected region in BUFFER.

Returns a cons cell - `(beg . end)'"
  (with-current-buffer (or buffer (current-buffer))
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (cons (point-min) (point-max)))))

(provide 'robby-utils)

;;; robby-utils.el ends here
