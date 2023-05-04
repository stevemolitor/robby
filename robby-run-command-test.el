;;; robby-run-command-test.el  --- robby-run-command tests  -*- lexical-binding:t -*-

;;; Commentary:

;; Integration tests for robby-run-command

(require 'robby-run-command)
(require 'robby-actions)
(require 'robby-prompts)

;;; Code:

(robby-run-command
 :prompt #'robby-get-prompt-from-minibuffer
 :action #'robby-respond-with-message)

(provide 'robby-run-command-test)

;; robby-run-command-test.el ends here
