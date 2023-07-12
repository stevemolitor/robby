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
(cl-defun robby-prepend-response-to-region (&key text beg chars-processed &allow-other-keys)
  "Prepend AI response to region, or buffer if no selected region."
  (when (eq chars-processed 0)
    (goto-char beg)
    (insert "\n"))
  (goto-char (+ beg chars-processed))
  (insert (format "%s" text)))

;;;###autoload
(cl-defun robby-append-response-to-region (&key text end response-buffer chars-processed completep &allow-other-keys)
  "Append AI response to region, or buffer if no selected region."
  (with-current-buffer (or response-buffer (current-buffer))
    (when (eq chars-processed 0)
      (goto-char end)
      (insert "\n"))
    (goto-char (+ 1 end chars-processed))
    (insert (format "%s" text))))

(cl-defun robby-append-response-to-region (&key text end chars-processed completep &allow-other-keys)
  "Append AI response to region, or buffer if no selected region."
  (when (eq chars-processed 0)
    (goto-char end)
    (insert "\n"))
  (goto-char (+ 1 end chars-processed))
  (insert (format "%s" text)))

;;;###autoload
(cl-defun robby-replace-region-with-response (&key text beg end chars-processed &allow-other-keys)
  "Append AI response to region, or buffer if no selected region."
  (when (eq chars-processed 0)
    (delete-region beg end))
  (goto-char (+ beg chars-processed))
  (insert text))

(provide 'robby-actions)

;;; robby-actions.el ends here

