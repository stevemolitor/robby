;;; robby-prompts.el  --- robby prompt functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to get prompts for OpenAI.

;;; Code:

(require 'cl-lib)

(cl-defun robby-get-prompt-from-minibuffer (&rest)
  "Get Robby prompt from minibuffer."
  (read-string "Request for AI overlords: "))

(cl-defun robby--get-region-or-buffer-text (&optional buffer)
  "Get Robby prompt from BUFFER region.

If no selected region return all text in buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun robby-get-prompt-from-region (&key prompt-buffer prompt-prefix prompt-suffix never-ask-p &allow-other-keys)
  "Get prompt from region.

Get prompt from selected region in PROMPT-BUFFER, or entire
buffer if no selected region.

If supplied PROMPT-PREFIX and/or PROMPT-PREFIX are prepended or
appended to the buffer or region text to make the complete
prompt.

If there both PROMPT-PREFIX and PROMPT-SUFFIX are nil or not
specified, prompt the user for a prompt prefix in the minibuffer.

If NEVER-ASK-P is non-nil, do not prompt the user for a prefix."
  (let* ((prompt-from-region (robby--get-region-or-buffer-text (or prompt-buffer (current-buffer))))
         (prefix (cond
                  (prompt-prefix prompt-prefix)
                  (prompt-suffix nil)
                  ((not never-ask-p) (read-string "Request for AI overlords: "))
                  (t nil))))
    (format "%s%s%s"
            (if prefix (concat prefix "\n") "") ; prefix
            prompt-from-region          ; region or buffer text
            (if prompt-suffix (concat "\n" prompt-suffix) ""))))

(provide 'robby-prompts)

;;; robby-prompts.el ends here
