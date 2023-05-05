;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

(require 'robby-run-command)
(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)

;;; Code:

(robby-define-command
 robby-help-window
 "Query AI from minibuffer, respond in help window."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-in-help-window)

(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message)

(robby-define-command
 robby-prepend-region
 "Query AI from region or entire buffer if no selected region,
prepend results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-prepend-response-to-region)

(robby-define-command
 robby-append-region
 "Query AI from region or entire buffer if no selected region,
append results to region or buffer."
  :prompt #'robby-get-prompt-from-region
 :action #'robby-append-response-to-region)

(robby-define-command
 robby-replace-region
 "Query AI from region or entire buffer if no selected region,
replace region with response."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-replace-region-with-response)

(provide 'robby-commands)

;;; robby-commands.el ends here
