;;; robby-commands.el  --- robby autoloaded custom commands  -*- lexical-binding:t -*-

;;; Commentary:

;; Built in Robby commands.

(require 'robby-define-command)
(require 'robby-prompts)
(require 'robby-actions)

;;; Code:

;;;###autoload (autoload 'robby-view "robby-commands" nil t)
(robby-define-command
 robby-view
 "Query AI from minibuffer, respond in robby-view-mode buffer."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-robby-view
 :action-args `(:response-buffer ,robby--view-buffer)
 :historyp t)

;;;###autoload (autoload 'robby-view-from-region "robby-commands" nil t)
(robby-define-command
 robby-view-from-region
 "Query AI from minibuffer, respond in robby-view-mode buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-respond-with-robby-view
 :action-args `(:response-buffer ,robby--view-buffer)
 :historyp t)

;;;###autoload (autoload 'robby-conversation "robby-commands" nil t)
(robby-define-command
 robby-conversation
 "Start AI conversation."
 :prompt #'robby-get-prompt-from-minibuffer-with-stop-message
 :action #'robby-respond-in-conversation
 :historyp t)

;;;###autoload (autoload 'robby-message "robby-commands" nil t)
(robby-define-command
 robby-message
 "Query AI from minibuffer, respond with message."
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message
 :historyp t
 :never-stream-p t)

;;;###autoload (autoload 'robby-prepend-region "robby-commands" nil t)
(robby-define-command
 robby-prepend-region
 "Query AI from region or entire buffer if no selected region,
prepend results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-prepend-response-to-region)

;;;###autoload (autoload 'robby-append-region "robby-commands" nil t)
(robby-define-command
 robby-append-region
 "Query AI from region or entire buffer if no selected region,
append results to region or buffer."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-append-response-to-region)

;;;###autoload (autoload 'robby-confirm-replace-region "robby-commands" nil t)
(robby-define-command
 robby-replace-region
 "Query AI from region or entire buffer if no selected region,
replace region with response.

If prefix arg is supplied, confirm changes in a diff buffer
before applying."
 :prompt #'robby-get-prompt-from-region
 :action #'robby-replace-region-with-response)

(provide 'robby-commands)

;;; robby-commands.el ends here
