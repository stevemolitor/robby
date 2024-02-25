;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)
(require 'robby-grounding-fns)

;;; Code:

;;;###autoload (autoload 'robby-view "robby-commands" "Query AI from minibuffer, respond in robby-view-mode buffer." t)
(robby-define-command
 robby-view
 "Query AI from minibuffer, respond in robby-view-mode buffer."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-robby-view
 :historyp t)

;;;###autoload (autoload 'robby-view-from-region "robby-commands" "Query AI from minibuffer, respond in robby-view-mode buffer." t)
(robby-define-command
 robby-view-from-region
 "Query AI from region, respond in robby-view-mode buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-respond-with-robby-view-without-prompt
 :historyp t)

;;;###autoload (autoload 'robby-message "robby-commands" "Query AI from minibuffer, respond with message." t)
(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message
 :grounding-fns #'robby-format-message-text
 :historyp t
 :never-stream-p t)

;;;###autoload (autoload 'robby-prepend-region "robby-commands" "Query AI from region or entire buffer if no selected region, prepend results to region or buffer." t)
(robby-define-command
 robby-prepend-region
 "Query AI from region or entire buffer if no selected region,
prepend results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-prepend-response-to-region
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

;;;###autoload (autoload 'robby-append-region "robby-commands" "Query AI from region or entire buffer if no selected region, append results to region or buffer." t)
(robby-define-command
 robby-append-region
 "Query AI from region or entire buffer if no selected region,
append results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-append-response-to-region
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

;;;###autoload (autoload 'robby-replace-region "robby-commands" "Query AI from region or entire buffer if no selected region, replace region with response." t)
(robby-define-command
 robby-replace-region
 "Query AI from region or entire buffer if no selected region,
replace region with response.

If prefix arg is supplied, confirm changes in a diff buffer
before applying."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-replace-region-with-response
 :grounding-fns #'robby-extract-fenced-text-in-prog-modes
 :never-stream-p t)

(provide 'robby-commands)

;;; robby-commands.el ends here
