;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

(require 'robby-run-command)
(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-view)

;;; Code:

(robby-define-command
 robby-view
 "Query AI from minibuffer, respond in robby-view-mode buffer."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-robby-view
 :historyp t)

(robby-define-command
 robby-conversation
 "Start AI conversation."
 :prompt #'robby-get-prompt-from-minibuffer-with-stop-message
 :action #'robby-respond-in-conversation
 :historyp t)

(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message
 :historyp t)

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
