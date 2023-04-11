;;; robby-save-commands.el  --- function to save robby custom commands   -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-insert-last-command' function, which inserts the
;; last command macro into the current buffer.

;;; Code:

(defvar robby--last-command-options nil
  "Last robby command.")

(defun robby-insert-last-command (name)
  "Insert a definition for the last command invoked into current
buffer.

NAME specifies the new command name, a symbol."
  (interactive "sCommand name: ")
  (let* ((docstring (read-string "Doc string: "))
         (cmd `(robby-define-command ,(intern name) ,docstring ,@robby--last-command-options)))
    (insert (format "%S" cmd))))

(provide 'robby-save-commands)

;;; robby-save-commands.el ends here
