;;; robby-prompts.el  --- robby prompt functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Functions to get prompts for OpenAI.

;;; Code:

(defun robby-get-prompt-from-minibuffer ()
  "Get Robby prompt from minibuffer."
  (read-string "Request for AI overlords: "))

(defun robby-get-prompt-from-minibuffer-with-stop-message ()
  "Get Robby prompt from minibuffer, with instructions on how to quit."
  (interactive)
  (read-from-minibuffer
   (concat "Request for AI overloards (" (propertize (substitute-command-keys "\\<global-map>\\[keyboard-quit]" t) 'face '(help-key-binding default)) " to stop): ")))

(cl-defun robby--get-region-or-buffer-text (&optional buffer)
  "Get Robby prompt from buffer region.

If no region return all text in buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun robby-get-prompt-from-region (&key prompt-buffer prompt-prefix prompt-suffix never-ask-p &allow-other-keys)
  "Get prompt from region, or entire buffer if no selected
 region.

If supplied PROMPT-PREFIX and/or PROMPT-PREFIX are prepended or
appended to the buffer or region text to make the complete
prompt.

If there both PROMPT-PREFIX and PROMPT-SUFFIX are nil or not
specified, prompt the user for a prompt prefix in the minibuffer.

If NEVER-ASK-P is t, do not prompt the user for a prompt prefix
no matter what."
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
