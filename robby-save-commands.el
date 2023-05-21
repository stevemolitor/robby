;;; robby-save-commands.el  --- function to save robby custom commands   -*- lexical-binding:t -*-

;;; Commentary:

;; Provides `robby-insert-last-command' function, which inserts the
;; last command macro into the current buffer.

;;; Code:

(defvar robby--last-command-options nil
  "Last robby command.")

(cl-defun robby--save-last-command-options (&key prompt prompt-args action action-args historyp api api-options)
  (setq robby--last-command-options
        `(:prompt
          ,(if (functionp prompt) `#',prompt ,prompt)
          :prompt-args
          ',prompt-args
          :action
          ,`#',action
          :action-args
          ',action-args
          :historyp
          ,historyp
          :api
          ,api
          :api-options
          ',api-options)))

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
