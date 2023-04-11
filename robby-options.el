;;; robby-options.el  --- assemble OpenAI options  -*- lexical-binding:t -*-

;;; Commentary:

;;  `robby-options' function, which assembles options to pass to
;;  OpenAI from robby customization variables.

(require 'map)
(require 'seq)
(require 'cus-edit)

(require 'robby-customization)
(require 'robby-utils)

;;; Code:

(defun robby--remove-api-prefix (api string)
  "Remove api prefix API from STRING.
For example, \"robby-chat-temperature\" becomes \"temperature\""
  (let ((regexp (format "^robby-%s-" api)))
    (replace-regexp-in-string regexp "" string)))

(defun robby--kebab-to-snake-case (string)
  "Transform STRING from kebab to snake case.
Ie \"a-b-c\" becomes a_b_c."
  (replace-regexp-in-string "-" "_" string))

(defun robby--options (options)
  "Get a list of options to pass to the OpenAI API.

Grabs OpenAI customization options for the current API as
specified in the `robby-api' custom variable and merges them in
with any specific options passed in OPTIONS. OPTIONS overrides
customization options."
  (seq-sort-by
   #'car #'string<

   (map-merge
    'alist
    (seq-filter
     (lambda (elem) (not (null (cdr elem))))
     (seq-map
      (lambda (sym)
        (cons
         (robby--kebab-to-snake-case (robby--remove-api-prefix robby-api (symbol-name sym)))
         (symbol-value sym)))
      (seq-map
       #'car
       (seq-filter
        (lambda (elem) (eq (nth 1 elem) 'custom-variable))
        (custom-group-members (intern (format "robby-%s-api" robby-api)) nil)))))
    (seq-map
     (lambda (assoc) (cons (robby--kebab-to-snake-case (replace-regexp-in-string "^:" "" (symbol-name (car assoc)))) (cdr assoc)))
     (robby--plist-to-alist options)))))

(provide 'robby-options)

;;; robby-options.el ends here
