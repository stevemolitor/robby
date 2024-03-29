;;; robby-grounding-fns.el  --- robby grounding functions  -*- lexical-binding:t -*-

;;; Commentary:

;; A robby grounding filters the response from AI before presenting it to the user.

;;; Code:

(require 'rx)

(defun robby-extract-fenced-text (response)
  "Extract the text between the first pair of fenced code blocks in RESPONSE."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (let ((beg (re-search-forward "```.*$" nil t))
          (end (re-search-forward "```" nil t)))
      (if (and beg end)
          ;; remove any trailing newline before the end of the fence
          (let ((resp (buffer-substring-no-properties (+ beg 1) (- end 3))))
            (replace-regexp-in-string "\n$" "" resp))
        response))))

(defun robby-extract-fenced-text-in-prog-modes (response)
  "Extract fenced text.

Extract the text between the first pair of fenced code blocks in
RESPONSE if in a programming mode, else return RESPONSE."
  (if (derived-mode-p 'prog-mode)
      (robby-extract-fenced-text response)
    response))

(defun robby-format-message-text (response)
  "Replace % with %% in RESPONSE to avoid format string errors calling `message."
  (replace-regexp-in-string "%" "%%" response))

(defun robby-remove-trailing-end-of-line (string)
  "Remove the end of line character at the very end of STRING, if present."
  (replace-regexp-in-string "
$" "" string))

;;; use cl-defun to define a function that removes leading and trailing double quotes from a string, using rx
(defun robby-remove-quotes (string)
  "Remove leading and trailing double quotes from STRING."
  (replace-regexp-in-string (rx "\"" eol) "" (replace-regexp-in-string (rx bol "\"") "" string)))

(provide 'robby-grounding-fns)

;;; robby-grounding-fns.el ends here
