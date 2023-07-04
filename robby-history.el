;;; robby-history.el  --- robby OpenAI prompt / response history  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby conversation history

(require 'robby-customization)

;;; Code:

(defvar robby--history nil "AI Conversation history.")

(defun robby-clear-history ()
  "Clear robby conversation history."
  (interactive)
  (setq robby--history nil)
  (message "Robby history erased."))

(defun robby--history-push (prompt response)
  "Push PROMPT and RESPONSE onto `robby--history'."
  (let ((start-pos (max 0 (- (length robby--history) robby-max-history))))
    (setq robby--history (append (seq-subseq robby--history start-pos) `(,(cons prompt response))))))

(provide 'robby-history)

;;; robby-history.el ends here
