;;; robby-prefix-args.el  --- prefix arg query functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to determine what to do in Robby commands based prefix
;; arg.

;;; Code:

(defun robby--clear-history-p (arg)
  "Clear history based on ARG.

Returns true when ARG is \\[universal-argument]
\\[universal-argument] or \\[universal-argument]
\\[universal-argument] \\[universal-argument]."
  (let ((n (prefix-numeric-value arg)))
    (or (eq n 16) (eq n 64))))

(defun robby--preview-p (arg)
  "Preview in help buffer based on ARG.

Return true when arg is \\[universal-argument] or
\\[universal-argument] \\[universal-argument]
\\[universal-argument]."
  (or (eq (prefix-numeric-value arg) 4)
      (eq (prefix-numeric-value arg) 64)))

(provide 'robby-prefix-args)

;;; robby-prefix-args.el ends here
