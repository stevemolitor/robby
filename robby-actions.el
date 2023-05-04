;;; robby-actions.el  --- robby OpenAI API response actions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to handle OpenAI responses.  These functions can be used
;; as the `:action' parameter when defining custom robby commands via
;; `robby-define-command'.

(require 'robby-utils)

;;; Code:

(defvar robby--buffer "*robby*" "Robby help buffer name, for displaying OpenAI responses.")

;;;###autoload
(cl-defun robby-respond-with-message (&key text &allow-other-keys)
  "Show TEXT in minibuffer message."
  (message (robby--format-message-text text)))

;;;###autoload
(cl-defun robby-respond-in-help-window (&key text &allow-other-keys)
  "Show TEXT in help window."
  (with-help-window (get-buffer-create robby--buffer)
    (princ text)))

;;;###autoload
(cl-defun robby-prepend-response-to-region (&key text beg output-buffer &allow-other-keys)
  "Prepend AI response to region, or buffer if no selected region."
  (with-current-buffer (or output-buffer (current-buffer))
    (goto-char beg)
    (insert (format "%s\n" text))))

;;;###autoload
(cl-defun robby-append-response-to-region (&key text end output-buffer &allow-other-keys)
  "Append AI response to region, or buffer if no selected region."
  (with-current-buffer (or output-buffer (current-buffer))
    (goto-char end)
    (insert (format "\n%s" text))))

;;;###autoload
(cl-defun robby-replace-region-with-response (&key text beg end output-buffer)
  "Append AI response to region, or buffer if no selected region."
  (with-current-buffer (or output-buffer (current-buffer))
    (delete-region beg end)
    (goto-char beg)
    (insert text)))

(provide 'robby-actions)

;;; robby-actions.el ends here

