;;; robby-grounding-fns.el  --- robby grounding functions  -*- lexical-binding:t -*-

;;; Commentary:

;; A robby grounding filters the response from AI before presenting it to the user.

;;; Code:

(defun robby-extract-fenced-text (response)
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (let ((beg (re-search-forward "```.*$" nil t))
          (end (re-search-forward "```" nil t)))
      (if (and beg end)
          (buffer-substring-no-properties (+ beg 1) (- end 3))
        response))))

(defun robby-format-message-text (response)
  "Replace % with %% in TEXT to avoid format string errors calling `message."
  (replace-regexp-in-string "%" "%%" response))

(defun robby-no-op (no-op-pattern)
  "Return a grounding function that returns the original input if
OpenAI response matches NO-OP-PATTERN.

Useful when you want OpenAI to respond with specific text to
signal that no change should be applied. For example, if the
prompt is 'Fix this code. If the code is correct respond with
\"the code is correct\", then you would use `(robby-no-op \"the
code is correct\" to create a grounding function that returns the
original input when the response is \"the code is correct\"."
  (lambda (prompt response)
    (if (string-match no-op-pattern response)
        prompt
      response)))

(provide 'robby-grounding-fns)

;; robby-grounding-fns.el ends here
