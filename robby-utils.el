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

(defun robby--plist-to-transient-args (plist)
  "Convert PLIST to transient args list."
  (let ((alist (robby--plist-to-alist plist)))
    (seq-map
     (lambda (assoc)
       (let ((key (replace-regexp-in-string "^:" "" (symbol-name (car assoc)) 1))
             (value (cdr assoc)))
         (format "%s=%s" key value)))
     alist)))

(defun robby--string-to-sym (string)
  (intern (format ":%s" string)))

(defun robby--sym-to-string (sym)
  (replace-regexp-in-string "^:" "" (symbol-name sym)))

(defun robby--empty-p (thing)
  (or (null thing) (string= thing "")))

(provide 'robby-utils)

;;; robby-utils.el ends here
