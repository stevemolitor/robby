;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)

;;;###autoload (autoload 'robby-message "robby" "Query AI from minibuffer, respond with message" t)
;;; Code:

(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message"
 :prompt robby--get-prompt-from-minibuffer
 :action robby--respond-with-message
 :historyp t)

;;;###autoload (autoload 'robby-prepend-region "robby" "Query AI from region, prefix region with response." t)
(robby-define-command
 robby-prepend-region
 "Query AI from region, prefix region with response.

Prompt for extra context with prefix arg.
If no selected region read prompt from buffer content."
 :prompt robby--get-prompt-from-region
 :action robby--prepend-response-to-region)

;;;###autoload (autoload 'robby-append-region "robby" "Query AI from region, append region to response." t)
(robby-define-command
 robby-append-region
 "Query AI from region, append region to response.

Prompt for extra context with prefix arg.
If no selected region read prompt from buffer content."
 :prompt robby--get-prompt-from-region
 :action robby--append-response-after-region)

;;;###autoload (autoload 'robby-replace-region "robby" "Query AI from region, replace region with response." t)
(robby-define-command
 robby-replace-region
 "Query AI from region, replace region with response.

Prompt for extra context with prefix arg.
If no selected region read prompt from buffer content."
 :prompt robby--get-prompt-from-region
 :action robby--replace-region-with-response)

;;;###autoload (autoload 'robby-help-window "robby" "Query AI from region, replace region with response." t)
(robby-define-command
 robby-help-window
 "Query AI from region, respond in help window.

Prompt for extra context with prefix arg.
If no selected region read prompt from buffer content."
 :prompt robby--get-prompt-from-region
 :action robby--show-response-in-help-window)

(provide 'robby-commands)

;;; robby-commands.el ends here
