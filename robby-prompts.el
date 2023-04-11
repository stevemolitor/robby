;;; robby-prompts.el  --- robby prompt functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to get prompts for OpenAI.

(require 'robby-prefix-args)

;;; Code:

(defun robby--get-prompt-from-minibuffer (arg)
  "Get prompt from minibuffer.

If ARG is t also clear robby history."
  (if (robby--prompt-for-extra-context-or-clear-history-p arg)
      (robby-clear-history))
  (let ((prompt (read-string "Request for AI overlords: ")))
    `(,prompt . ,prompt)))

(defun robby--get-region-or-buffer-text ()
  "Get text in selected region.

If no region return all text in buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun robby--get-prompt-from-region (arg)
  "Get prompt from region, or entire buffer if no selected
 region.

Prompt for extra context if `ARG'."
  (let ((prompt-from-region
         (robby--get-region-or-buffer-text)))
    (if (robby--prompt-for-extra-context-or-clear-history-p arg)
        (let* ((prompt-prefix (read-string "Request for AI overlords: "))
               (prompt (format "%s\n%s" prompt-prefix prompt-from-region)))
          `(,prompt . ,prompt-prefix))
      `(,prompt-from-region . ,prompt-from-region))))

(provide 'robby-prompts)

;;; robby-prompts.el ends here
