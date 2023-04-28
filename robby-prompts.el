;;; robby-prompts.el  --- robby prompt functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to get prompts for OpenAI.

(require 'robby-prefix-args)

;;; Code:

(defun robby--get-prompt-from-minibuffer (arg)
  "Get prompt from minibuffer.

If ARG is t also clear robby history."
  (let ((prompt (read-string "Request for AI overlords: ")))
    `(,prompt . ,prompt)))

(defun robby--get-region-or-buffer-text (&optional buffer)
  "Get text in selected region.

If no region return all text in buffer."
  (with-current-buffer (or buffer (current-buffer))
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (buffer-substring-no-properties (point-min) (point-max)))))

;; TODO don't prompt with custom commands
(defun robby--get-prompt-from-region ()
  "Get prompt from region, or entire buffer if no selected
 region.

Ask the user for a prompt prefix to prepend to the region that is
sent as the OpenAI prompt."
  (let* ((prompt-from-region (robby--get-region-or-buffer-text))
         (prompt-prefix (read-string "Request for AI overlords: "))
         (prompt (format "%s\n%s" prompt-prefix prompt-from-region)))
    `(,prompt . ,prompt-prefix)))

(provide 'robby-prompts)

;;; robby-prompts.el ends here
