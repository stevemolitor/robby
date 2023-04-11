;;; robby-actions.el  --- robby OpenAI API response actions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to handle OpenAI responses.  These functions can be used
;; as the `:action' parameter when defining custom robby commands via
;; `robby-define-command'.

(require 'robby-utils)

;;; Code:

(defvar robby--buffer "*robby*" "Robby help buffer name, for displaying OpenAI responses.")

;;;###autoload
(defun robby--respond-with-message (text _beg _end)
  "Show TEXT in minibuffer message."
  (message (robby--format-message-text text)))

;;;###autoload
(defun robby--show-response-in-help-window (text _beg _end)
  "Show TEXT in help window."
  (with-help-window (get-buffer-create robby--buffer)
    (princ text)))

;;;###autoload
(defun robby--prepend-response-to-region (text beg _end)
  "Insert TEXT at position BEG."
  (goto-char beg)
  (insert (format "%s\n" text)))

;;;###autoload
(defun robby--append-response-after-region (text _beg end)
  "Insert TEXT at position END."
  (goto-char end)
  (insert (format "\n%s" text)))

;;;###autoload
(defun robby--replace-region-with-response (text beg end)
  "Replace region between BEG and END with TEXT."
  (delete-region beg end)
  (goto-char beg)
  (insert text))

(provide 'robby-actions)

;;; robby-actions.el ends here
